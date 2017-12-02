---
title: Controlling MPlayer
tags: linux
---

## MPlayer

MPlayer is an extremely versatile media player, I've begun to use
it for absolutely any media that I'm not already piping through
mpd. One day while going through my XMonad config, I decided it'd
be convenient to bind my media keys to control MPlayer. I already
had them bound to control volume/mpd, but I figured Meta + key
combinations could be the MPlayer equivalents.

A bit of googling later and I had the solution: a fifo!

## Fifos

Fifos (for *file in file out*) are two way files on your system that can be used
for communication; kind of a poor man's `socket`. You can play with them like
this to get the idea:

```
# in one terminal:
mkfifo ./fifo
tail -f ./fifo

# and in some other terminal:
echo some text > ./fifo
```

## MPlayer setup

The MPlayer manpage states that it can read commands out of a fifo
by using the input flag. Combine that with the fact that MPlayer
will read any flags from `~/.mplayer/config` and we're 90% there.

    mkfifo ~/.mplayer_fifo
    vim ~/.mplayer/config

Add the following in that file:

    input = file=/home/username/.mplayer_fifo

Now fire up a movie. Go to *some other* terminal and do the
following:

    echo pause > ~/.mplayer_fifo

If MPlayer didn't pause, double check the above. It works for me.

## Keybinds

Now it's really up to you if you want to run these via a wrapper
script, or send the commands directly from your keybind
configuration. Here's an example wrapper script if you decide to go
this way:

```bash 
#!/bin/bash

fifo="$HOME/.mplayer_fifo"
command="$*"

echo $command > "$fifo" &>/dev/null
```

Place it in your `$PATH`, `chmod +x` it, and bind some keys to
`script 'play'`, `script 'pause'`, etc.

Personally, I put a simple function (of basically the above) in my
`xmonad.hs`, then call that from the keybinds. Here's the relevant
section of my config:

```haskell 
myKeys = [ ...

         -- Mod+ to control MPlayer
         , ("M-<XF86AudioPlay>", mPlay "pause"   ) -- play/pause mplayer
         , ("M-<XF86AudioStop>", mPlay "stop"    ) -- stop mplayer
         , ("M-<XF86AudioPrev>", mPlay "seek -10") -- seek back 10 seconds
         , ("M-<XF86AudioNext>", mPlay "seek 10" ) -- seek forward 10 seconds

         , ...
         ] 

         where

           mPlay s = spawn $ "echo " ++ s ++ " > $HOME/.mplayer_fifo"
```

<div class="well">
I'm using 
[EZConfig](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html) 
notation in my keybindings.
</div>

I'll leave it up to you to figure out your WM's keybind
configuration or use some generic tool like xbindkeys.
