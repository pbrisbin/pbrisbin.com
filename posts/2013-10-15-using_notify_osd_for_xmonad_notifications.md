---
title: Using Notify-OSD for XMonad Notifications
tags: haskell, xmonad
---

In my continuing efforts to strip my computing experience of any 
non-essential parts, I've decided to ditch my statusbars. My desktop is 
now solely a grid of tiled terminals (and a browser). It's quite nice. 
The only thing I *slightly* missed, however, was notifications when one 
of my windows set Urgency. This used to trigger a bright yellow color 
for that workspace in my dzen-based statusbar.

<div class="well">
**A Brief Tangent**:

Windows have these properties called "hints" which they can set on 
themselves at will. These properties can be read by Window Managers in 
an effort to do the Right Thing. Hints are how a Window tells the 
Manager, "Hey, I should be full-screen" or, "I'm a dialog, float me on 
top of everything". One such hint is `WM_URGENT`.

`WM_URGENT` is how windows get your attention. It's what makes them 
flash in your task bar or bounce in your dock. If you're using a sane 
terminal, it should set `WM_URGENT` on itself if the program running 
within it triggers a "bell".

By telling applications like mutt or weechat to print a bell when I get 
new email or someone nick-highlights me, I can easily get notifications 
of these events even from applications that are running within screen, 
in an ssh session, on some server far, far away. Pretty neat.
</div>

Now that I'm without a status bar, I need to be notified some other way. 
Enter Notify-OSD.

## Notify-OSD

Notify-OSD is part of the desktop notification system of GNOME, but it 
can be installed standalone and used to send notifications from the 
command-line very easily:

```
$ notify-send "A title" "A message"
```

So how do we get XMonad to send a useful notification via `notify-send` 
whenever a window sets the `WM_URGENT` hint? Enter the `UrgencyHook`.

## UrgencyHook

Setting a custom urgency hook is very easy, but not exactly intuitive. 
What we're actually doing is declaring a custom data type, then making 
it an instance of the `UrgencyHook` typeclass. The single required 
function to be a member of this typeclass is an action which will be run 
whenever a window sets urgency. Conveniently, it's given the window with 
urgency as an argument. We can use this to format our notification.

First off, add the module imports we'll need:

```haskell
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.Run

import qualified XMonad.StackSet as W
```

Then make that custom datatype and instance:

```haskell
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset

        safeSpawn "notify-send" [show name, "workspace " ++ idx]
```

Finally, update `main` like so:

```haskell
main :: IO ()
main = xmonad
     $ withUrgencyHook LibNotifyUrgencyHook
     $ defaultConfig
        { -- ...
        , -- ...
        }
```

To test this, open a terminal in some workspace and type:

```
$ sleep 3 && printf "\a"
```

Then immediately focus away from that workspace. In a few seconds, you 
should see a nice pop-up like:

![notify-send](https://images.pbrisbin.com/using_notify_osd_for_xmonad_notifications/notify-send.png)\ 

You can see the title of the notification is the window name and I use 
the message to tell me the workspace number. In this case, the name is 
the default "urxvt" for a terminal window, but I also use a few wrapper 
scripts to open urxvt with the `-n` option to set its name to something 
specific which will then come through in any notifications from that 
window.

If that doesn't work, it's likely your terminal doesn't set Urgency on 
bells. For rxvt at least, the setting is:

```
URxvt*urgentOnBell: true
URxvt*visualBell:   false
```

In `Xresources` or `Xdefaults`, whichever you use.
