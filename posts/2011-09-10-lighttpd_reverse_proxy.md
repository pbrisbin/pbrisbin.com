---
title: Lighttpd Reverse Proxy
tags: haskell, self, yesod
---

This site was previously served via lighttpd using fastcgi. My haskell 
source was compiled using `Network.Wai.Handler.FastCGI` and the binary 
was placed at `/srv/http/app.cgi` to be handled by lighttpd's 
`mod_fastcgi`.

I decided to switch it up and let Warp serve the haskell app directly, 
then proxy certain urls through to it via lighttpd.

This how-to will outline the steps needed to get this setup and comment 
a little bit on what all the moving parts do.

<div class="well">
This guide assumes your code is structured roughly like the 0.9.1 
scaffolder, and your `Application.hs` exports that `withYourApp` 
function which is used by `main.hs` and compiled into a binary and 
executed.

My application is called "DevSite" (I don't know why), so anywhere you 
see that in this guide, just assume I mean your foundation type / app 
name.
</div>

## Why

Compiling to fastcgi was starting to feel kind of icky. Warp is all 
grown up now and capable of serving content mighty quickly. Often a 
problem with my app would result in lighttpd silently failing and 
leaving troublesome pid files around.

It's nicer to let that front-facing server sit there running, 
none-the-wiser that I'm constantly developing and recompiling the app 
that it's forwarding to.  Installing and starting a compiled Warp binary 
will give me greater feedback in the event something goes awry.

Fortunately, I already had the backbone of url-rewriting going on to get 
requests to `app.cgi` so I just needed to update that to pull http 
traffic from another port on localhost rather than actually call a CGI 
process to get the response.

Lighttpd 1.5 built the proxy framework with the intention of 
superseding `mod_fastcgi` and providing that feature simply by telling 
you to proxy to a fastcgi application in the same way you would to 
another domain. This meant I just had to update my syntax for 1.5, then 
it was almost as easy as `s/fastcgi/http/`ing the config.

There's also the minor benefit that I no longer need duplicated support 
files (like client-session key and favicon) between development and 
production.

## The Moving Parts

Lighttpd will rewrite / redirect urls in a few stages:

1. Certain urls will be handled by lighttpd itself. I like lighttpd for 
   static file serving. It's got a pretty directory listing, it's fast, 
   and it makes it super easy to setup a `/static/private` which 
   enforces simple http-auth for access -- very handy.

2. Everything else will be rewritten (once) to `/proxy/$1`.

3. Anything coming in for `/proxy/.*` (presumably via rewrite) will go 
   to another port on localhost where my Warp server will take over.

Lighttpd can also load-balance over multiple instances of Warp (nifty!).

## The Setup

First let's get lighttpd setup. I'm using `mod_proxy_core` which is only 
available in lighttpd-1.5+. If you're on Arch, you can install 
`aur/lighttpd-svn`.

Import some modules:

```bash 
server.modules = ( "mod_rewrite"
                 , "mod_proxy_core"
                 , "mod_proxy_backend_http"
                 )
```

Setup the "stage-one" redirects:

```bash 
# notice that by rewriting to /proxy$1 and not /proxy/$1 we get the 
# desired behavior where / becomes /proxy/ and /what/ever becomes 
# /proxy/what/ever.
url.rewrite-once = ( "^/static.*" => "$0"
                   , "(.*)"       => "/proxy$1"
                   )
```

Finally, setup the actual proxying:

```bash 
$HTTP["url"] =~ "^/proxy.*" {
  # straight, http pass-through
  proxy-core.protocol        = "http"

  # lighttpd will manage its own queue and send requests to whichever 
  # instance has the shortest queue
  proxy-core.balancer        = "sqf"

  # these are the 5 Warp instances we'll start
  proxy-core.backends        = ( "127.0.0.1:3001"
                               , "127.0.0.1:3002"
                               , "127.0.0.1:3003"
                               , "127.0.0.1:3004"
                               , "127.0.0.1:3005"
                               )

  # strip the /proxy prefix
  proxy-core.rewrite-request = ( "_uri" => ( "^/proxy(.*)" => "$1" ) )
}
```

Now that we've got that going we need to spin up some Warp instances to 
serve out anything lighttpd redirects from `/proxy`.

Luckily the scaffolded `main.hs` allows us to pass a port on the command 
line, so we'll just start up a bunch of instances of our app all 
listening on a different port.

## Script It Out

I like to script this process of starting and stopping the multiple Warp 
instances. To facilitate this, we need to create some support 
directories alongside your source code:

    mkdir tmp/{pid,log}

With those in place, feel free to take the following functions and 
incorporate them into some server management script:

```bash 
instances='1 2 3 4 5'

start_devsite() {
  local n

  echo 'Starting worker processes...'
  for n in $instances; do
    devsite -p=300$n > tmp/log/$n.log 2> tmp/log/${n}_errors.log &
    echo $! > tmp/pid/$n.pid
  done
}

stop_devsite() {
  local pid n

  echo 'Stopping worker processes...'
  for n in $instances; do
    if [[ -f tmp/pid/$n.pid ]]; then
      read -r pid < tmp/pid/$n.pid
      if [[ -n $pid ]]; then
        kill $pid
        rm tmp/pid/$n.pid
      fi
    fi
  done
}
```

Once you execute the start function, you should see 5 processes running 
listening on ports 3001 through 3005. Lighttpd is already setup to 
forward to those apps in a load-balanced way so go ahead and see if it 
worked!
