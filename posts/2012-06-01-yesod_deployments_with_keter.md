---
title: Yesod Deployments with Keter
tags: haskell, yesod
---

[Keter][keter] is Michael Snoyman's answer to the Yesod deployment 
"problem". This is something I've been looking for a good solution to 
for some time now and keter does a really great job.

[keter]: https://github.com/snoyberg/keter "Keter"

Keter is meant to run as a service. It sets up its own workspace and 
watches for keter "bundles" to be dropped into an `incoming/` directory. 
A keter bundle is just a gzipped tarball of your app's executable, any 
support files and directories it might expect to have in its current 
working directory, and a configuration file to tell keter a few simple 
things about how to manage it.

When a bundle is found in `incoming/`, keter will:

* Create a folder to run the app in

* Run the configured executable for the app, setting environment 
  variables so it listens on a random port

* Capture the process's output to a log in the keter workspace

* Setup an nginx virtual host entry which reverse proxies to the app and 
  tell nginx to reload

* If configured to do so, setup the postgresql database that your app 
  needs

Keter also manages multiple versions of the same app through a 
zero-downtime deployment. It will bring up a new version in its own 
folder and wait until the current version is done serving requests 
before sending it a `SIGTERM` and removing its folder.

Though this guide will focus on getting a yesod application deploying 
with keter on Arch Linux, attempts will be made to explain things in a 
general enough way that the instructions will allow you to get this 
going on any distro.

This guide also assumes you've got postgresql setup and working and will 
manage it outside of keter. Basically, you've already got a running site 
and a (possibly sub-optimal) method of deployment -- I'm going to cover 
the transition to a keter-based approach.

## Keter

First of all, install keter. At the time of this writing, we need to run 
the git version since it contains the code needed to customize the nginx 
start/reload commands.

```
$ git clone https://github.com/snoyberg/keter
$ cd keter
$ cabal configure
$ cabal build
$ sudo cp dist/build/keter/keter /usr/bin/keter
```

The last step is optional, just know that you'll need the keter binary 
somewhere in root's `$PATH`.

Next, we'll setup a configuration file to tell keter where to place its 
working files and how to start and reload nginx.

**/etc/keter.yaml**:

```yaml 
root: /opt/keter
nginx:
  start:
    - /etc/rc.d/nginx
    - start
  reload:
    - /etc/rc.d/nginx
    - reload
```

And a file to run keter as a service:

**/etc/rc.d/keter**:

```bash 
#!/bin/bash

. /etc/rc.conf
. /etc/rc.d/functions

PIDFILE=/var/run/keter.pid

case "$1" in
  start)
    stat_busy 'Starting Keter'
    keter /etc/keter.yaml &>/dev/null &
    echo $! >"$PIDFILE"

    if [[ $? -gt 0 ]]; then
      stat_fail
    else
      add_daemon keter
      stat_done
    fi
    ;;

  stop)
    stat_busy 'Stopping Keter'
    read -r pid < $PIDFILE

    kill $pid || kill -9 $pid

    if [[ $? -gt 0 ]]; then
      stat_fail
    else
      rm_daemon keter
      stat_done
    fi
    ;;

  restart)
    $0 stop
    sleep 3
    $0 start
    ;;
  *)
    echo "usage: $0 {start|stop|restart}"
esac
exit 0
```

Don't start keter just yet, we've got a few more steps.

## Nginx

If you've already got a site being reversed proxied to by nginx, that's 
good, but it's likely that keter will complete this task differently 
than you're currently doing it. We'll manually update our configs to the 
"keter way" first, so the transition to keter goes as smoothly as 
possible.

You've probably got everything in a single config file; we're going to 
modularize by site. Keter will write a `server` block to 
`/etc/nginx/sites-enabled/keter` containing the reverse proxy 
declaration. There's no reason we can't get setup that way now and 
verify it's working without keter's involvement.

**/etc/nginx/conf/nginx.conf**

```
user you;
worker_processes 1;

events {
  worker_connections 1024;
}

http {
  # you can run multiple sites by setting up any number of files in 
  # sites-enabled and having each respond to a specific server_name, 
  # your keterized apps will just be one of them.
  include /etc/nginx/sites-enabled/*;
}
```

**/etc/nginx/sites-enabled/keter**

```
server {
    listen 80;
    server_name example.com
    location / {
       # keter will use a dynamic port in the 4000s, if you let your 
       # current setup use something outside that range you can leave 
       # your current app running when you start keter for the first 
       # time. that way, if it doesn't work, you're no worse off than 
       # you were before.
       proxy_pass http://127.0.01:3001
    }
}
```

<div class="well">
It's been my experience that starting only the keter service will not 
then bring up nginx. Not sure if this is intended or a bug; just be 
aware that you need to start the nginx service yourself. Keter only 
seems to handle sending the `reload` command on deployments.
</div>

## Your App

Now we are ready to keterize our app! All it really takes is one 
additional config file:

**config/keter.yaml**

```yaml 
exec: ../dist/build/yourapp/yourapp
args:
  - production
host: example.com
```

I also write a small script to handle the process of building the app 
and placing the tarball in keter's incoming directory:

**config/deploy**

```bash 
#!/bin/bash -ex

cabal clean
cabal configure
cabal build

strip dist/build/yourapp/yourapp

rm -rf static/tmp/

# you can use this to tar directly into the incoming folder, but you 
# need write access to it
tar czfv - dist/build/yourapp/yourapp config static > /opt/keter/incoming/yourapp.keter

# if you don't want to provide a normal user that access, you can split 
# the command and use sudo on the mv
tar czfv yourapp.keter dist/build/yourapp/yourapp config static
sudo mv yourapp.keter /opt/keter/incoming/
```

## Try it

Finally, let's try it out:

```bash 
# Start the keter service:
sudo /etc/rc.d/keter start

# Tail the log in a separate terminal so you can see any problems
tail -f /opt/keter/log/keter/current.log

# Deploy!
./config/deploy
```

You should see output like the following in the `tail`ing terminal:

```
2012-06-01 14:42:07.85: Unpacking bundle '/opt/keter/incoming/yourapp.keter' into folder: /opt/keter/temp/yourapp-0
2012-06-01 14:42:08.54: Created process: config/../dist/build/yourapp/yourapp
2012-06-01 14:42:10.55: App finished reloading: yourapp
```

And `/etc/nginx/sites-enabled/keter` should've been overwritten with 
something like:

```
server {
    listen 80;
    server_name example.com;
    location / {
       proxy_pass http://127.0.0.1:4003;
       proxy_set_header X-Real-IP $remote_addr;
    }
}
```

Make sure your site's still working and you're all set!

At this point you can kill off any old version you might've had running 
and go on developing and deploying at will simply by dropping new keter 
bundles.

## Systemd

If you've made the switch to systemd, there are only a few differences 
compared to above.

First of all, change the keter config file to use the newer commands:

**/etc/keter.yaml**:

```yaml 
root: /opt/keter
nginx:
  start:
    - systemctl
    - start
    - nginx.service
  reload:
    - systemctl
    - reload
    - nginx.service
```

Secondly, rather than creating an rc.d file, create a (much simpler) 
service file

**/etc/systemd/system/keter.service**

```
[Unit]
Description=Keter Deployment Handler
After=local-fs.target network.target

[Service]
ExecStart=/usr/bin/keter /etc/keter.yaml

[Install]
WantedBy=multi-user.target
```

<div class="well">
Recently, a post of mine made it to the front page of Hacker News and I 
was bombarded with traffic for about 5 hours. Aside from the general 
network slowness of serving from behind a residential Comcast IP, the 
site held up surprisingly well. CPU and Memory were no issue. One 
problem I did run into however was file handles.

Turns out, systemd limits any service it manages to 4096 file handles by 
default. So, if you expect to get decent traffic, it can be a good idea 
to increase this. Adding `LimitNOFILE=<number>` to the `[Service]` block 
above does the trick. The special value `infinity` is also available.
</div>

Finally, use the following to start the service and enable it at boot.

```
# systemctl start keter.service
# systemctl enable keter.service
```

## Benefits

There are a couple of non-obvious benefits to the keter system:

1. It works equally well for local or remote servers

If you're deploying your app to a remote server just (have keter running 
there and) change your deployment script to end with:

```bash 
tar czfv - dist/build/yourapp/yourapp config static |\
  ssh example.com 'cat > ~/keter/incoming/yourapp.keter'
```

2. It works equally well for non-yesod apps too

The only real requirement is that the executable respect the `$PORT` 
environment variable when choosing how to listen. This is becoming an 
increasingly popular pattern with hosted solutions like heroko and 
nodester so any well-behaved app should probably do this anyway.

Besides that, you've just got to make a proper bundle: a 
`config/keter.yaml`, your executable and any other files or directories 
your app expects to have present in its current directory.

## Downsides

Keter is in its early stages of development so it's not without its 
failings. Mainly, it's not very flexible -- you're expected to use the 
nginx reverse proxy approach with a single executable backend server.

You're also unable to setup any static file serving tricks at this time 
(though there is code in Keter to handle it, and I've been playing with 
some ideas in my own fork).

Those issues notwithstanding, I'm still finding the approach incredibly 
streamlined and useful for both my local deployments of pbrisbin.com and 
some remote servers I deploy to. I was able to ditch a number of 
scattered service files and bash scripts that had been hobbled together 
to fill this gap.

Well done Michael.
