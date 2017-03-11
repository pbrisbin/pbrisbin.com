---
title: XMonad Scratchpad
tags: haskell, xmonad
---

It's been a while since I've made an XMonad post. Thought a good one might be
details regarding the scratchpad extension from `-contrib`.  This can be
confusing to set up, but oh-so useful. If you've ever used a quake (or yakuake?)
terminal (I have not), you'll know what I'm talking about. It's a small terminal
that sits idle on a non-visible workspace. You can call it up with a quick
keybind, use it for whatever, then banish it away again with the same keybind.

You just have to use it for a while to realize how useful it really is. My goal
for this post is to distill out of my `xmonad.hs` just the scratchpad
functionality so that someone with an existing `xmonad.hs` could easily plug
this into their setup with minimal fuss.

## Prerequisites

I'm going to assume that your existing `xmonad.hs` defines a function 
called `myManageHook` and another called `myTerminal`. If this is not 
the case, take a look at the below snippet; I think you'll be able to 
figure out how to rework whatever you do have into this format.

```haskell 
main = do
  xmonad $ defaultConfig
    { terminal   = myTerminal
    , manageHook = myManageHook
    , ...
    , ...
    }

myTerminal = "urxvt"

-- you could have some crazy long managehook 
-- or simply defaultManageHook
myManageHook = ...
```

## Imports

You'll need the following import to make this functionality available. Make sure
you've got -contrib installed and add the following to the top of your
xmonad.hs:

```haskell 
import XMonad.Util.Scratchpad
```

## ManageHook

We're going to add an additional `ManageHook` to manage the scratch-pad
specifically. In XMonad, we can compose `ManageHook`s together in a few ways.
The easiest is probably to combine whatever you currently have with our new one
via `<+\>`, and infix function of type `ManageHook -> ManageHook -> ManageHook`:

```haskell
-- depending on what you have, parens may or may not be needed
myManageHook = (...) <+> manageScratchPad

-- then define your scratchpad management separately:
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)

  where

    h = 0.1     -- terminal height, 10%
    w = 1       -- terminal width, 100%
    t = 1 - h   -- distance from top edge, 90%
    l = 1 - w   -- distance from left edge, 0%
```

What I've done is used `RationalRect` to define a rectangle of where I'd like
the scratch-pad to appear. `h`, `w`, `t`, and `l` are entered as percentage
screen size. So in the above, I've got a rectangle that spans the monitor's
entire width and is 10% its height. By specifying `h` and `w`, `t` and `l` can
be derived to place it on the bottom edge of the screen.

## KeyBinds

I'm not really going to get specific with the key binding part. 
Personally, I use EZConfig. Everyone seems to have their own 
syntax/style of binding keys in xmonad; usually it's just the way it was 
in the first config you copied from, whatever. Just know that 
someway-somehow you'll need to bind a key to...

```haskell 
myKeys = [ ( ... , ...        )
         , ( ... , scratchPad ) -- spawn a scratchpad terminal
         ]

         where 

           scratchPad = scratchpadSpawnActionTerminal myTerminal
```

Make sense?

## Extra credit

At this point, you should have a functioning scratchpad. Remember, any 
changes to the manageHook require you to exit and reopen the scratchpad 
terminal to see the effect.

Using this scratchpad module creates a workspace called NSP where the 
scratchpad resides when it's not visible. You'll notice, this workspace 
will show up in any dzen or xmobar you've got going on.  But with some 
changes to our logHook we can filter that out of the workspace list 
pretty easily.

If you're not using a custom logHook, you've pretty much got two choices 
at this point: head over to the docs on xmonad.org and find some drop-in 
filter-out-NSP module and figure out how add it (I know it's there but I 
could not for the life of me get it working), or just figure out how to 
get a custom logHook going.

What I'm about to go into assumes you've already got something like the 
following defined in your xmonad.hs:

```haskell 
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor color1 color2 . pad
  , ppVisible         = dzenColor color1 color2 . pad
  , ppUrgent          = dzenColor color1 color2 . pad . dzenStrip
  , ppLayout          = dzenColor color1 color2 . pad
  , ppHidden          = dzenColor color1 color2 . pad
  , ppHiddenNoWindows = namedOnly
  , ppTitle           = shorten 100 
  , ppSep             = " "
  , ppWsSep           = ""
  , ppOutput          = hPutStrLn h
  }
```

<div class="well">
The above requires other contrib modules, changes to main, and special 
imports to get working. As I've said, I'm leaving it as an exercise for 
the reader to set up her own logHook.
</div>

Once we've got this, filtering out the NSP workspace is pretty straight 
forward. Here's the above again, but this time with the NSP workspace 
filtered out, hopefully you'll be able to modify things as needed to 
make this work with your setup.

```haskell 
myLogHook h = dynamicLogWithPP $ defaultPP
  { ppCurrent         = dzenColor color1 color2 . pad
  , ppVisible         = dzenColor color1 color2 . pad
  , ppUrgent          = dzenColor color1 color2 . pad . dzenStrip
  , ppLayout          = dzenColor color1 color2
  , ppLayout          = dzenColor color1 color2 . pad
  , ppHidden          = dzenColor color1 color2 . pad . noScatchPad -- haskell makes it so easy,
  , ppHiddenNoWindows = noScratchPad                                -- just tack on another function
  , ppTitle           = shorten 100 
  , ppSep             = " "
  , ppWsSep           = ""
  , ppOutput          = hPutStrLn h
  }

  where
    -- then define it down here: if the workspace is NSP then print
    -- nothing, else print it as-is
    noScratchPad ws = if ws == "NSP" then "" else ws
```

Good luck!
