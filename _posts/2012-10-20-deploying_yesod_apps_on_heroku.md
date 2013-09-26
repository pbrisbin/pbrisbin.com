---
layout: post
title: "Deploying Yesod Apps On Heroku"
tags:
  - yesod
  - website
  - ops
  - cloud
---

<div class="well">
**Update** This post describes compiling a Yesod application locally 
using a VM to achieve the compilation on a Heroku-like machine, then 
pushing the binary up to Heroku to be run. This is an annoying route 
which is no longer necessary (as mentioned in the comments), so don't 
follow it. Instead follow [this][] guide.
</div>

[this]: http://brianmckenna.org/blog/haskell_buildpack_heroku

The following are the steps I followed to get a non-trivial Yesod 
application running on Heroku.

<div class="well">
This guide assumes you know what Heroku is, you've got the Toolbelt 
installed, and your ssh keys are set up. The wiki I followed can be 
found [here][wiki]. The Heroku "Getting started" guides were also very 
useful.
</div>

[wiki]: https://github.com/yesodweb/yesod/wiki/Deploying-Yesod-Apps-to-Heroku

## Setup

Create an app for your project:

```
$ heroku apps:create
```

Add a Procfile

```
$ cat Procfile
web: ./myapp production -p $PORT
```

Add a package.json

```
$ cat package.json
{
  "name"         : "myapp",
  "version"      : "0.0.0",
  "dependencies" : {}
}
```

<div class="well">
The package.json file tricks Heroku into running us as if we were a 
node.js app which really just means executing the command in the 
Procfile.
</div>

## Postgesql

Add the add-on:

```
$ heroku addons:add heroku-postgresql
```

Then "promote" your database, whatever that means...

```
$ heroku pg:info
=== HEROKU_POSTGRESQL_ORANGE_URL (DATABASE_URL)
Plan:        Dev
Status:      available
Connections: 0
PG Version:  9.1.6
Created:     2012-10-20 02:28 UTC
Data Size:   6.1 MB
Tables:      0
Rows:        0/10000 (In compliance)
Fork/Follow: Unavailable

$ heroku pg:promote ORANGE
```

Grab the credentials information:

```
$ heroku pg:credentials ORANGE
Connection info string:
   "dbname=dfh57p6tk1gqbl 
   host=ec2-54-243-228-169.compute-1.amazonaws.com port=5432 user=yphlhbhmzthocg password=4KX6f7tENj2YaAh43vWoCqfMAo sslmode=require"
```

And translate them into your postgresql.yml:

```yaml 
Production:
  <<: *defaults
  user: yphlhbhmzthocg
  password: 4KX6f7tENj2YaAh43vWoCqfMAo
  host: ec2-54-243-228-169.compute-1.amazonaws.com
  port: 5432
  database: dfh57p6tk1gqbl
  sslmode: require
```

<div class="well">
As mentioned in the comments, putting credentials for a world-reachable 
database into publicly shared source code is a Bad Idea. In my case, the 
applications I place on Heroku are throw away prototypes for which this 
lack of security is perfectly acceptable.

Please consider carefully your own security needs.

This (untested) [gist][] may work for pulling the database credentials 
from the environment.
</div>

[gist]: https://gist.github.com/pbrisbin/5156677

## Build

<div class="well">
If your local hardware doesn't match Heroku's, **you're gonna have a bad 
time**
</div>

There is a great pre-packaged Vagrant setup for Haskell floating around 
bitbucket, but I found it was a bit broken. I made the needed changes to 
get it working and the resulting fork is available [here][].

[here]: https://github.com/pbrisbin/vagrant-haskell

Add it as a sub directory within your project:

```
$ git clone https://github.com/pbrisbin/vagrant-haskell ./vagrant
```

Use it to compile your binary:

```
$ cd ./vagrant
$ vagrant up
$ vagrant ssh
[guest]$ cabal update
[guest]$ cabal install cabal-install
[guest]$ cd /app
[guest]$ cabal install --only-dependencies
[guest]$ cabal configure -fproduction
[guest]$ cabal build
```

<div class="well">
These steps will take a long time the first time around because you're 
compiling GHC, the Haskell Platform, then installing all your Yesod 
dependencies. As long as you don't destroy the VM, subsequent rebuilds 
won't have to repeat those steps.
</div>

## Deploy

I keep `dist` out of version control, so I just move the binary up to 
top-level and commit it there:

```
$ cp dist/build/myapp/myapp .
$ git add ./myapp
$ git commit -m 'add binary'
```

Deploy to Heroku:

```
$ git push heroku master
```

Read the output from the push then go view your site.

If you get an Application error when viewing your freshly deployed site, 
you can check to see what's wrong via `heroku logs`. I direct you back 
to the original [wiki][] for some trouble shooting tips.

<div class="well">
Pushing to Heroku requires you setup SSH keys (like any hosting service 
should). When you initially `heroku login` it will look for an existing 
key and use it or create a default `id_rsa.pub` for you.

I actually prefer to have separate per-service keys (`id_rsa.github`, 
`id_rsa.nodester`, `ide_rsa.heroku`, etc). This lets me use 
password-less keys for these less-critical logins and still have my main 
`id_rsa` be password-protected for logging into my own servers.

So here's what I do:

```
$ ssh-keygen
Generating public/private rsa key pair.
Enter file in which to save the key (/home/patrick/.ssh/id_rsa): /home/patrick/.ssh/id_rsa.heroku
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
$ heroku keys:clear
$ heroku keys:add /home/patrick/.ssh/id_rsa.heroku.pub
```

Then add the following to `~/.ssh/config`:

```
Host heroku.com
  IdentityFile ~/.ssh/id_rsa.heroku
```

I have similar entries for the other services I mentioned.
</div>

## Automate

Now that you've gone through this process once, you should automate it 
via a simple deploy script:

```bash 
#!/bin/bash -ex
(
  cd ./vagrant

  vagrant up
  vagrant ssh -c 'cd /app &&
                  cabal clean &&
                  cabal configure -fproduction &&
                  cabal build'
)

cp dist/build/myapp/myapp .
git commit -m ./myapp 'new binary'

git push heroku master
```

## Bonus: DNS

If you own a domain which you would like to point to this Heroku app, 
the easiest way I've found is to use the Zerigo DNS add-on:

```
$ heroku addons:add zerigo_dns:basic
```

<div class="well">
The add-on is free, but they do require you verify your account and add 
billing information to install it.
</div>

Update your Domain Registrar to use their name servers:

```
a.ns.zerigo.net
b.ns.zerigo.net
```

And add your domain

```
$ heroku domains:add mydomain.com
```

Wait for DNS to propogate, and you're done.
