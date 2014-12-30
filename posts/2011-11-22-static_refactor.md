---
title: Static Refactor
tags: website
---

Just a quick heads-up post about a recent site refactoring.

I decided to switch to nginx from lighttpd, let it do the static file 
serving, and at the same time drop all the complicated redirects I'd 
been carrying since going live on yesod. I also cleaned out the 
`/static` directory a little bit and streamlined its folder structure.

Below please find info about the deprecated routes that I've finally 
dropped (and some that were dropped a while ago).

### No longer redirecting old 8080 links

Please use [pbrisbin.com](http://pbrisbin.com) to view the site.

### No longer redirecting /dotfiles and /bin to github

Please see [github](https://github.com/pbrisbin) for all of my configs 
and other projects.

### No longer redirecting \*.rss to /feed

Please use [pbrisbin.com/feed/](http://pbrisbin.com/feed/) for my rss.

### Removed /music

Please email if you really were interested in that stuff.

### Rearranged documentation folders

*Note: the documentation subdomain has since been removed entirely.*

Haskell docs (including xmonad libraries) are at docs.pbrisbin.com/haskell and
ruby docs are at docs.pbrisbin.com/ruby.

I think that's it -- let me know if I've missed something and I'll add a 
note here.
