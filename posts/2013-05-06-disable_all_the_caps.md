---
title: Disable All The Caps
tags: arch, linux, udev
---

If you're like me and absolutely abhor the Caps Lock key, you've 
probably figured out some way to replace it with a more suitable 
function. I myself have settled on the following command to make it a 
duplicate `Ctrl` key:

```
$ setxkbmap -option ctrl:nocaps
```

This works great when placed in `~/.xinitrc` and run as X starts, but 
what about USB keyboards which are plugged in later? Perhaps your 
pair-programming, or moving your laptop between work and home. This 
happens frequently enough that I thought it'd be nice to use a udev rule 
to trigger the command auto-magically whenever a keyboard is plugged in.

The setup is fairly simple in the end, but I found enough minor traps 
that I thought it was appropriate to document things once I got it 
working.

<div class="well">
It has come to my attention that configuring this via `xorg.conf.d` 
actually does affect hot-plugged keyboards.

**/etc/X11/xorg.conf.d/10-keyboard.conf**

```
Section "InputClass"
  Identifier "Keyboard Defaults"
  MatchIsKeyboard "yes"
  Option "XkbOptions" "ctrl:nocaps"
EndSection
```

While this renders the rest of this post fairly pointless, it is a much 
cleaner approach.
</div>

## Script

You can't just place the `setxkbmap` command directly in a udev rule 
(that'd be too easy!) since you'll need enough decoration that a 
one-liner gets a bit cumbersome. Instead, create a simple script to add 
this decoration; then we can call it from the udev rule.

Create the file wherever you like, just note the full path since it will 
be needed later:

**~/.bin/fix-caps**

```bash 
#!/bin/bash
(
  sleep 1
  DISPLAY=:0.0 setxkbmap -option ctrl:nocaps
) &
```

And make it executable:

```
$ chmod +x ~/.bin/fix-caps
```

Important things to note:

1. We `sleep 1` in order to give time for udev to finish initializing 
   the keyboard before we attempt to tweak things.
2. We set the `DISPLAY` environment variable since the context in which 
   the udev rule will trigger has no knowledge of X (also, the `:0.0` 
   value is an assumption, you may need to tweak it).
3. We background the whole command with `&` so that the script returns 
   control back to udev immediately while we (wait a second and) do our 
   thing in the background.

## Rule

Now that we have a single callable script, we just need to run it (as 
our normal user) when a particular event occurs.

**/etc/udev/rules.d/99-usb-keyboards.rules**

```
SUBSYSTEM=="input", ACTION=="add", RUN+="/bin/su patrick -c /home/patrick/.bin/fix-caps"
```

Be sure to change the places I'm using my username (patrick) to yours. I 
had considered putting the `su` in the script itself, but eventually 
decided I might use it outside of udev when I'm already a normal user. 
The additional line-noise in the udev rule is the better trade-off to 
me.

And again, a few things to note:

1. I don't get any more specific than the subsystem and action. I don't 
   care that this runs more often then actually needed.
2. We need to use the full path to `su`, since udev has no `$PATH`.

## Testing

There's no need to reload anything (that happens automatically). To 
execute a dry run via `udevadm test`, you'll need the path to an input 
device. This can be copied out of `dmesg` from when one was connected or 
you could take an educated guess.

Once that's known, execute:

```
# udevadm test --action=add /dev/path/to/whatever/input0
...
...
run: '/bin/su patrick -c /home/patrick/.bin/fix-caps'
...
```

As long as you see the `run` bit towards the bottom, you should be all 
set. At this point, you could unplug and re-plug your keyboard, or tell 
udev to re-process events for currently plugged in devices:

```
# udevadm trigger
```

This command doesn't need a device path (though I think you can give it 
one); without it, it triggers events for all devices.
