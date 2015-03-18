---
title: Status Bars in XMonad
tags: dzen, haskell, xmonad
---

One of the trickiest things for a new user of XMonad is adding a
statusbar. This is mainly because xmonad's statusbar support is so
transparent and extensible, that any documentation for setting it up
could be completely different from any other. Do you want a dynamicLog?
A customLog? xmobar? dzen? One bar? Two?

Here I'll outline my method. Two separate dzen2 bars aligned to look
like one bar across the top of your screen. The left fed by an xmonad
[dynamicLogWithPP][] to show workspaces (with coloring and
[urgencyHooks][]), the current layout and the current window title. The
right fed by conky to show music, system stats and of course the time.

<div class="well">
Many thanks go to moljac and lifeafter2am on the Arch forums. They
offered their xmonad.hs's to me and helped get me setup this way.
</div>

## What it looks like

Full desktop:

![XMonad Shot](https://images.pbrisbin.com/xmonad_statusbars/status.png)\ 

And with an `urgencyHook` notification (Workspace turns a different
color):

![XMonad Shot Urgent](https://images.pbrisbin.com/xmonad_statusbars/status-urgency.png)\ 

To achieve this, we set up a `dynamicLog` in `xmonad.hs` and adjust our
`main` function to output to this bar and also spawn our right bar as
fed by conky.

## Imports and the Main function

Your imports and main function will look like this:

```haskell
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

main = do
    -- spawn our left and right bars. in my case, I use two monitors,
    -- I want one bar on each, and my version of dzen supports the -xs
    -- argument for specifying on which screen to appear. if your
    -- situation is different in some way, use -w and -x to give your
    -- bars appriate width and x offsets for your needs.
    d <- spawnPipe "dzen2 -p -xs 1 -ta l -e 'onstart=lower'"

    spawn $ "conky -c ~/.xmonad/data/conky/dzen | " ++
                "dzen2 -p -xs 2 ta -r -e 'onstart=lower'"

    xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig
        { ...
        , logHook = myLogHook d

        -- having these call out to external functions makes it easier 
        -- to add the "no overlap" stuff later on. if you don't have 
        -- myLayoutHook or myManageHook, you can continue to use the 
        -- xmonad defaults by declaring them like so:
        -- 
        -- > myManageHook = manageHook defaultConfig
        -- > myLayoutHook = layoutHook defaultConfig
        -- 
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        }
```

Don't worry about the things we haven't defined yet, I'll get to those.
Also, the conky config file which I use can be found in my [xmonad 
repo][].

[xmonad repo]: https://github.com/pbrisbin/xmonad-config/blob/old-master/data/conky/dzen

## Your LogHook

Your `logHook` will setup the output of workspaces, layouts, and titles
to the left `dzen`. You can customize the formatting, padding,
shortening, etc.

Here's a commented version of `myLogHook` which, hopefully, is
illustrative enough to not warrant further explanation.

```haskell
-- 
-- Loghook
-- 
-- note: some of these colors may differ from what's in the
-- screenshot, it changes daily
-- 
myLogHook h = dynamicLogWithPP $ defaultPP

    -- display current workspace as darkgrey on light grey (opposite of 
    -- default colors)
    { ppCurrent         = dzenColor "#303030" "#909090" . pad 

    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad 

    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 

    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 

    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip

    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100

    -- no separator between workspaces
    , ppWsSep           = ""

    -- put a few spaces between each object
    , ppSep             = "  "

    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    }
```

## No Overlap

The last thing you should do is add two little things to make sure you
leave a gap for the new statusbar:

```haskell
-- add avoidStruts to your layoutHook like so
myLayoutHook = avoidStruts $ {- whatever you had before... -}

-- add manageDocks to your managehook like so
myManageHook = manageDocks <+> {- whatever you had before ... -}
```

Happy haskelling!

[dynamicLogWithPP]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-DynamicLog.html
[urgencyHooks]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-UrgencyHook.html
