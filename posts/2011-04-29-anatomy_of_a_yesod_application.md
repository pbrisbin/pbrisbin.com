---
title: Anatomy of a Yesod Application
tags: haskell, yesod, self
---

*subtitle: how to stay sane when developing for the web in haskell*

<div class="well">
This post was originally about how I structure my Yesod applications and 
where it differs from the scaffold tool. I've since done a bit of a 180 
and started to really like the scaffold tool and its structure.

For that reason, I've rewritten the post to outline *that* structure, 
its benefits and some strategies I use above what it provides to develop 
and deploy Yesod applications.

Note that this information is 0.8-specific and with the 0.9 and 1.0 
versions of Yesod, this post will be obsolete (until I update it again).
</div>

I recently had the chance to do a coding exercise for a job interview. 
Due to some misunderstandings of the task on my part I had to do it 
twice. I'm still embarrassed about it, but I did end up getting the job 
so all's well that ends well...

Anyway, the cool thing about doing it twice is that the first time, I 
did it in Rails and the second time in Yesod and that gave me a chance 
to evaluate the two frameworks in somewhat of a side-by-side.

I'm not going to get into a big discussion on the pros and cons of each 
one in general -- though I will say they both excelled at staying out of 
my way and getting me a base for the exercise very quickly. What I'd 
rather talk about is structure.

During my brief time hacking in Rails, I quickly grew to like the 
"convention over configuration" philosophy. If you create a `model/foo` 
and a `controller/foo` and a `test/foo`, then `foo` itself just 
magically works. I liked it.

I hadn't used the Yesod scaffold tool since about 0.1, but I knew I 
needed to get a site up quickly so I decided to use it for this 
exercise. I found that the new structure was very organized and well 
thought out. It gave me a similar convention-driven feeling, create 
`hamlet/foo` and `cassius/foo` then `widgetFile foo` would just work.

I think the framework could use a bit more of this approach to make the 
yesod-scaffolding tool as versatile as the ruby one. Mechanisms like 
`widgetFile` could be more plentiful and library provided (rather than 
scaffolded into your `Settings.hs`).

The yesod scaffold basically built you an "example" site which you can 
rewrite to your needs. In contrast, the ruby scaffold tool(s) let you 
say "I have some data structure like X" and it goes and creates a bunch 
of code to make X work. You're obviously still going to rewrite a lot of 
the generated code, but it's not a 100% guarantee like with `yesod 
init`.

Putting on my yesod-end-user cap (vs the usual yesod-contributer one), I 
would love to see yesod work more like rails: `yesod init` should give 
you a simple status-page site with links to all the documentation (you 
could still chose tiny, or database-driven and get all that setup at 
this point too). Then, `yesod scaffold --whatever` commands could be 
used to build up a CRUD interface with your actual data types.

<div class="well">
Hmm, that turned into a bit of a wine about how rails is better than 
yesod -- that is not my opinion in general. There are tons of reasons I 
prefer yesod overall, I was just really impressed with rails scaffolding 
abilities.
</div>

## Scaffold

Enough comparison, let's talk about yesod as it is now.

The scaffold tool sets up the following basic structure:

    /path/to/site
    |-- config
    |   `-- ...
    |-- hamlet
    |   `-- ...
    |-- cassius
    |   `-- ...
    |-- julius
    |   `-- ...
    |-- Handlers
    |   `-- ...
    |-- Model.hs
    |-- YourSite.hs
    |-- Controller.hs
    `-- yoursite.cabal

`config` is going to hold your `Settings.hs` module along with some text 
files where you define your routes and models. I also like to throw the 
main executables' source files in there which I'll discuss later.

`hamlet`, `cassius`, and `julius` will contain the templates files for 
html, css, and javascript respectively. One awesome new development is 
the aforementioned function `widgetFile` which I use 100% of the time 
regardless of what templates the page actually calls for. If you write, 
say, `addWidget $(widgetFile "foo")`, that will splice in the templates 
`hamlet/foo.hamlet`, `cassius/foo.cassius`, and `julius/foo.julius` and 
just ignores non-existent files.

`Model.hs`, `YourSite.hs`, and `Controller.hs` are pretty 
self-explanatory, and are either entirely site-dependant or 
scaffold-generated so I'm not going to discuss them.

One other cool feature of the scaffold is how it sets up the imports and 
exports in `YourSite.hs`. It handles all of the major imports (like 
`Yesod` itself, etc) and those that need to be `qualified` (like 
`Settings`) and then reexports them as a single clean interface. This 
means that all of your `Handlers`, `Helpers`, etc can just `import 
YourSite` and be done with it. Very nice, very clean.

`Handlers` usually contains one module per route and only defines the 
route handling functions. I try to keep any support functions in either 
per-handler or site-wide `Helpers`.

One last note: do yourself a favor and keep/maintain the generated 
cabal file. It's a nice way to prevent breakage (when dependencies are 
updated) and keep dev vs prod options straight. It's also nice to keep 
all the object and interface files hidden under an ignorable `dist` 
directory.

## Development

For development, I use an easy `simple-server` approach. The haskell is 
as follows:

```haskell 
import Controller (withServer)
import System.IO (hPutStrLn, stderr)
import Network.Wai.Middleware.Debug (debug)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
    let port = 3000
    hPutStrLn stderr $ "Application launched, listening on port " ++ show port
    withServer $ run port . debug
```

I then keep a simple shell script that runs it:

```bash 
#!/bin/bash -e

touch config/Settings.hs
runhaskell -Wall -iconfig config/simple-server.hs
```

The `touch` just ensures that anything set by `CPP` options are up to 
date every time I `./devel`.

## Deployment

By using the cabal file, deployments are pretty easy. I use lighttpd as 
my server-of-choice (I also let it do the static file serving), so I 
need to compile to fastcgi.

<div class="well">
I keep exactly one copy of any static files (including my main css) and 
it lives only in  the production location. To support this, I define a 
`staticLink` function in `Settings.hs` which is conditional on the 
`PRODUCTION` flag.

If I'm developing locally, `staticLink "foo"` would return 
`http://the-real-domain/static/foo` so that the file is linked from its 
live location. When running in production, that function just returns 
`/static/foo` which is what I would actually want in the html.

I find this approach is way simpler than any other way I've done static 
file serving.
</div>

My cabal file builds an executable from `config/mysite.hs` which looks 
like this:

```haskell 
import Controller (withServer)
import Network.Wai.Handler.FastCGI (run)

main :: IO ()
main = withServer run
```

Then I've got another shell script to make deployments a single-command 
operation:

```bash 
#!/bin/bash -e

app="${1:-/srv/http/app.cgi}"

sudo true

# this command just adds an auto-incrementing git tag so that if there's 
# some issue, I can just checkout the last tag and redeploy. this 
# completely sidesteps the need to backup the binary itself
deptag

touch config/Settings.hs
cabal install --bindir=./

# cabal will install to ./myapp as defined in the cabal file so we just 
# stop the service and replace the binary
sudo /etc/rc.d/lighttpd stop
sudo mv myapp "$app"
sudo /etc/rc.d/lighttpd start
```

This approach can be easily extended to a non-local deployment. In the 
case of rentersreality, the site lives on a slicehost. Its deployment 
file looks like this:

```bash 
#!/bin/bash -e

ip="${1:-rentersreality.com}"

deptag # tag deployments

touch config/Settings.hs
cabal install --bindir=./

scp ./renters "$ip":~/

ssh -t "$ip" '
  sudo /etc/rc.d/lighttpd stop        &&
  sudo mv ./renters /srv/http/app.cgi &&
  sudo /etc/rc.d/lighttpd start       &&
  sleep 3
'
```

I found that after executing the remote command I had to `sleep` so that 
the process could detach correctly. Things would end up in a bad state 
if I disconnected right away.

I must say, since moving to the more structured approach and utilizing 
cabal install as the main deployment step, I have had far less issues 
with developing and deploying my apps.

To see two sites that are currently using this structure, just browse 
[the][] [projects][] on my github.

[the]:      https://github.com/pbrisbin/devsite         "This site's source"
[projects]: https://github.com/pbrisbin/renters-reality "Another project of mine"
