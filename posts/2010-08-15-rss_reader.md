---
title: Haskell RSS Reader
tags: haskell, xmonad
---

I've been looking for a good Haskell project for a while now. The
language is just awesome, and I've been getting more and more
comfortable with it lately thanks to reading
[Real World Haskell](http://book.realworldhaskell.org/). I even
got the opportunity to write some haskell for a project at work
(I'm a consultant on a Microsoft product, crazy).

I wanted something challenging but doable; something to keep me
interested but still stretch my abilities. I had made some smaller
utilities to manage the pages on my site, so I was getting familiar
with parsing XML using some haskell libraries as well as starting
to wrap my head around the IO Monad a bit more. Well, I just
completed (what I think is) a slick little RSS reader using just
haskell and dzen.

For those that don't know, RSS feeds are basically just site
headlines; a very simple XML page that lists items, each item
containing a title, description, and link.

So my reader would read in a listing of feed urls, put together all
of the RSS items from each url, and then display them using dzen.

I put it in the upper right of my left monitor, configured to look
like part of my existing dzen status bars.

The title text remains static and is clickable (opens the url of
the feed item), and the description text is a ticker text that
rolls by right-to-left one character at a time.

## Installation

First, you would have to download `RssReader.hs` and `Dzen.hs` from
my old xmonad [library][] and place them in a directory along side a 
file called `rssreader.hs`. This file would serve the same purpose 
`xmonad.hs` does for XMonad: it would be both a configuration file and 
the main application itself, gluing together imported functions into a 
runnable `main`.

[library]: https://github.com/pbrisbin/xmonad-config/tree/old-master/lib

Here's an example:

```haskell 
import Dzen
import RssReader

-- 
-- this is it, the whole application in one line!
-- 

main :: IO ()
main = spawnDzen dzenConf >>= spawnReader readerConf

-- 
-- and the configuration part...
-- 

-- set a width and some text formatting
readerConf :: ReaderConf
readerConf = defaultReaderConf
  { titleFormat = dzenFG "#909090"
  , descrFormat = shorten 200 
  , tickerWidth = 150 
  }

  where
    -- some helpers
    dzenFG c s  = concat ["^fg(", c, ")", s, "^fg()"]
    shorten n s = if length s > n then (take n s) ++ "..." else s

-- start with the default dzen and override some things
dzenConf :: DzenConf
dzenConf = defaultDzen
  { x_position  = Just $ Percent 60 -- start 60% across screen 0
  , width       = Just $ Percent 40 -- and span the other 40%
  , font        = Just "Verdana-8"  -- if you have an xft-capable dzen
  , fg_color    = Just "#606060"
  , bg_color    = Just "#303030"
  }
```

Once that's all set, you can run
`ghc --make -o rssreader rssreader.hs` inside this directory to
create an executable which you can run standalone.

## Dependencies

The following packages would be required either from Hackage or
your distribution's package manager:

----------------------------------------
Hackage     Arch linux
----------- ----------------------------
http        extra/haskell-http

tagsoup     aur/haskell-tagsoup
----------------------------------------

## Known Issues

Some unprintable characters seem to still come through. I try to
clean the strings as much as possible, but I still see boxes in
dzen from time to time.

The rssreader and the spawned dzen are not tied together
process-wise. This means that you can kill rssreader and a frozen
dzen remains, or you can quit the dzen and rssreader will be left
as a zombie.
