---
layout: post
title: "XMonad Modules"
tags:
  - dzen
  - haskell
  - website
  - xmonad
---

This page is to serve as both an apology and an announcement. I've
recently modularized my `xmonad.hs`. I'm sorry.

<div class="well">
This is no longer true. I've since gone through a bit of a config
cleanse, deciding it makes my life easier to live closer to defaults and
not carry around a lot of extra configuration or features (that I don't
actively use).

As part of this cleanse, I've stripped my config back down to a very
lean `xmonad.hs` that can easily live within the confines of a single
file.
</div>

## Who cares?

I know of at least one person who stops by my site on a regular basis to
update his `xmonad.hs` to match the latest version of mine. I've also
seen, on a few occasions, someone mention that they use *brisbin33's*
xmonad config when discussing an issue on the forums or in IRC. True,
for all I know, there could be only three people using some form of my
config -- but to them, I'm sorry.

Anyone who blindly updates to my most recent `xmonad.hs` may get hit
with the following error:

      xmonad.hs:21:7:
          Could not find module `ScratchPadKeys':
             Use -v to see a list of the files searched for.

      Failed, modules loaded: none.

That's because I've offloaded some of the more module-ish chunks of my
config into, well, modules.

## Why?

I noticed, when browsing the XMonad source (I know, shut-up), that the
default recompile command includes the option `-ilib` this tells ghc to
include source files in `./lib`. It was a light-bulb moment.

I had gathered some pretty sophisticated code in my little `xmonad.hs`:
custom data types and instances, reusable utilities, etc. Why not put
them in their own files and import them into a nice clean config as I
would with any normal contrib module?

So, if you're following my `xmonad.hs`, please continue to do so. Just
be advised you'll need a few files in `lib` if you want to use the
functionality they offer.
