---
title: XMonad's IM Layout
tags: haskell, xmonad
---

One of my favorite modules from xmonad-contrib is the [IM layout][].
It's a tiling algorithm designed to handle your roster and chat windows
in the best way possible. Here I'm going to outline how I set this up in
my `xmonad.hs`.

## What it looks like

Personally, I want my roster tiled in its own properly-sized location on
one side, and all other IM related windows floating. I also want any of
those IM windows to automatically be pushed to the IM workspace.

![IM Layout Screenshot](http://images.pbrisbin.com/xmonads_im_layout/xmonad_im_layout.png)\ 

Here we can see that my roster is fit in its own little tile on the
left, sized properly. The rest of the screen is tiled as a grid of
whatever other applications I open. My chat window is always floating.

So, how would you set this up?

## Imports and Main

This post assumes you've imported the required modules and you have a
`main` function setup as shown:

```haskell
-- imports
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace

import qualified XMonad.StackSet as W

-- main
main = xmonad $ defaultConfig
    { ...
    -- all of our changes will take place in the myLayout and
    -- myManageHook definitions.
    , layoutHook = myLayout
    , manageHook = myManageHook
    }
```

## The Layout Hook

Here's a simple layoutHook that adds the IM extension on a specific
workspace and has the added bonus that you can cycle between all of your
"standard" layouts in the space that's not taken up by the roster.

Also, if your IM client isn't open, the workspace will behave like any
other.

```haskell
-- Layouts
myLayout = avoidStruts $ onWorkspace "3-im" imLayout $ standardLayouts

  where
    --          numMasters, resizeIncr, splitRatio
    tall = Tall 1           0.02        0.5

    -- define the list of standardLayouts
    standardLayouts = tall ||| Mirror tall ||| Full

    -- notice that withIM, which normally acts on one layout, can also 
    -- work on a list of layouts (yay recursive data types!)
    imLayout = withIM (1/10) (Role "roster") standardLayouts
```

I've defined the function `standardLayouts`, which we'll use on all
workspaces. Then for "3-im", I define `imLayout` which uses the `withIM`
modifier.

<div class="well">
This really highlights what XMonad brings as a WM, something you don't
get with most other tilers; because we are using haskell as the config
language, we have all kinds of native tricks at our disposal. The reason
the above works is that both `(someLayout)` and
`(someLayout |||  someOtherLayout ||| yetAnotherLayout)` are valid as
arguments to `withIM` due to their identical (existential) types. If the
compiler allows it, we can be pretty sure it'll behave as we intended.
</div>

Now you should have a nice IM layout setup, go ahead and `M-q`, then
head to workspace 3 (or whatever) and fire up your IM client

Feel free to stop here if you plan on having your IM Chat windows tiled.
I think that's kind of ugly, so I choose to have any IM-related window
*besides* the roster floated by default. My `manageHook` takes care of
that.

## The Manage Hook

```haskell
-- Manage hook
myManageHook = composeAll
    [ -- whatever you might already have, plus...

    -- move all IM windows to IM workspace
    , className =? "Gajim.py" --> doShift "3-chat"

    -- and float everything but the roster
    , classNotRole ("Gajim.py", "roster") --> doFloat
    ]

    where
        classNotRole :: (String, String) -> Query Bool
        classNotRole (c,r) = className =? c <&&> role /=? r

        role = stringProperty "WM_WINDOW_ROLE"
```

This will move all IM windows to the IM workspace, and float anything 
that's of the IM Class but not the roster's Role.

<div class="well">
You can use the commandline tool `xprop` to find out a window's
properties for use in these `manageHook`s.
</div>

[IM layout]: http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Layout-IM.html
