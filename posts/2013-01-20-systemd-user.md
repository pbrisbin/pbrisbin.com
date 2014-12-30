---
title: Systemd-User
tags: arch, systemd
---

<div class="well">
**BIG FAT WARNING**

One thing to note, and the reason why I'm no longer using this setup: 
screen sessions started from within X cannot survive X restarts. If you 
don't know what that means, don't worry about it; if you do, you've been 
warned.
</div>

A while back, Arch switched to [systemd][] for its init system. It's 
pretty enjoyable from an end-user perspective, unit files are far easier 
to write and maintain than the old rc-scripts, the process groups are 
conceptually consistent and robust, and the centralized logging via 
`journalctl` is pretty sweet.

[systemd]: http://en.wikipedia.org/wiki/Systemd

With a recent patch to dbus, it's now possible to run a second, 
user-specific instance of systemd to manage your login session. In order 
to describe why we might want to do this, and before we go into detail 
on how, it'd be useful to first talk about how a minimal graphical login 
session can be managed without it.

## Startx

When my machine first boots, I get a dead simple, tty-based login 
prompt. I don't use a display manager and consider this my graphical 
login.

When I enter my credentials, normal shell initialization proceeds no 
differently than any other time. When ZSH (my shell) gets to the end of 
`~/.zshenv` it finds the following:

```bash 
[[ $TTY == /dev/tty1 ]] \
  && (( $UID ))         \
  && [[ -z $DISPLAY ]]  \
  && exec startx
```

Translation: if I'm logging into the first physical tty, I'm not the 
root user, and there's no display already running, then start X.

<div class="well">
More specifically, due to the `exec` there, it *replaces itself with* X. 
Without this, someone would find themselves at a logged-in shell if they 
were to kill X -- something you can do even in the presence of most 
screen locks.
</div>

The `startx` command eventually sources `~/.xinitrc` where we find 
commands for initializing my X environment: wallpaper setting, a few 
`xset` commands, starting up `urxvtd`, etc. After all that, my window 
manager is started.

This is all well and good, but there are a few improvements we can make 
by letting systemd manage this process.

First of all, the output of All The Things is hard to find. It used to 
be that calling `startx` on tty1 would start an X session on tty7 and 
the output of starting up X and any applications launched in `xinitrc` 
would at least be spammed back on tty1. That seems to no longer be the 
case and `startx` starts X right there on tty1 hiding any output from 
those programs.

It's also hard to see all your X-related processes together as one 
cohesive group. Some processes would remain owned by `xinit` or `xmonad` 
(my window manager) but some would fork off and end up a direct child of 
`init`. Other "one shot" commands would run (or not) and exit without 
any visibility about their success or failure.

Using something like systemd can address these points.

## Systemd

Using systemd means setting up your own targets and service files under 
`~/.config/systemd/user` just as you do for your main system. With these 
service files in place, we can simply execute `systemd --user` and 
everything will be started up (in parallel) and managed by the new 
instance of `systemd`.

We'll be able to get useful status info about all the processes, manage 
them like system services, and see any output from them by using 
`journalctl`.

## Instructions

First, install `user-session-units` from the AUR, it'll also pull in 
`xorg-launch-helper`. This will provide us with a `xorg.target` which 
will handle getting X running.

Now, there's a bit of a chicken-and-the-egg problem we have to deal 
with. I ran into it when I first moved to systemd at the system level 
too. In order to have your services start automatically when you start 
systemd, you have to `enable` them. In order to `enable` them, you need 
systemd to be running. In this case it's a bit trickier since the user 
session can't start without one of those services we're going to enable, 
but we can't enable it without starting the user session...

The recommended way around this is to (temporarily) add `systemd --user 
&` to the top of your current `.xinitrc` and restart X.

<div class="well">
It's unclear to me if you could get away with just running that command 
from some terminal right where you are -- feel free to try that first.
</div>

Now that we're back with a user session running, we can set up our 
"services".

First, we'll write a target and service file for your window manager. I 
use XMonad, so mine looks like this:

**~/.config/systemd/user/xmonad.target**

```
[Unit]
Description=XMonad
Wants=xorg.target
Wants=xinit.target
Requires=dbus.socket
AllowIsolate=true

[Install]
Alias=default.target
```

**~/.config/systemd/user/xmonad.service**

```
[Unit]
Description=xmonad
After=xorg.target

[Service]
ExecStart=/home/you/.cabal/bin/xmonad
Environment=DISPLAY=:0

[Install]
WantedBy=xmonad.target
```

You can see we reference `xinit.target` as a `Want`, this target will 
hold all the services we used to start as part of `xinitrc`. Let's 
create the target for now, we'll worry about the services later:

**~/.config/systemd/user/xinit.target**

```
[Unit]
Description=Xinit
Requires=xorg.target
```

Then, enable our main target:

```
$ systemctl --user enable xmonad.target
```

This should drop a symlink at `default.target` setting that as the 
target to be run when you execute `systemd --user`.

At this point, if you were to quit X and run that command, it should 
successfully start X and load XMonad (or whatever WM you're using). The 
next thing we'll do is write service files for all the stuff you 
currently have in `xinitrc`.

Here are some of the ones I'm using as examples:

**~/.config/systemd/user/wallpaper.service**

```
[Unit]
Description=Wallpaper setter
After=xorg.target

[Service]
Type=oneshot
ExecStart=/usr/bin/feh --bg-tile %h/Pictures/wallpaper.png
Environment=DISPLAY=:0

[Install]
WantedBy=xinit.target
```

<div class="well">
It appears that we can use `%h` to represent our home directory, but 
only in certain ways. The above works, but trying to use `%h` in the 
path to the xmonad binary does not. Sigh.
</div>

**~/.config/systemd/user/synergys.service**

```
[Unit]
Description=Synergy Server
After=xorg.target

[Service]
Type=forking
ExecStart=/usr/bin/synergys --debug ERROR

[Install]
WantedBy=xinit.target
```

**~/.config/systemd/user/urxvtd.service**

```
[Unit]
Description=Urxvt Daemon
After=xorg.target

[Service]
Type=simple
ExecStart=/usr/bin/urxvtd

[Install]
WantedBy=xinit.target
```

With these in place, you can enable them all. I used the following 
shortcut:

```
$ cd .config/systemd/user
$ for s in *.service; do systemctl --user enable $s; done
```

Now, finally, simply running `systemd --user` should start X, bring up 
all your X-related services, and start your window manager.

How you do this going forward is up to you, but in my case I simply 
updated the last line in my `~/.zshenv`:

```bash 
[[ $TTY == /dev/tty1 ]] \
  && (( $UID ))         \
  && [[ -z $DISPLAY ]]  \
  && exec systemd --user
```

## Benefits

Arguably, we've complected what used to be a pretty simple system -- so 
what do we gain?

Well, my OCD *loves* seeing a nice, isolated process group of everything 
X-related:

![Process group](http://images.pbrisbin.com/systemd-user/htop-process-group.png)\ 

We can also now use a consistent interface for working with services at 
both the system and X level. Included in this interface is the super 
useful `status`:

![Service status](http://images.pbrisbin.com/systemd-user/urxvtd-service-status.png)\ 

Finally, we get the benefits of `journalctl` -- running it as our 
non-root user will show us all the messages from our X-related 
processes.

There are probably a number of additional `systemd` features that we can 
now leverage for our graphical environment, but I'm still in the process 
of figuring that all out.

## References

Many thanks to gtmanfred for putting this idea in my head and going 
through the hard work of figuring it out and [writing it up][gt-blog]. 
The information has also been added to the Arch [wiki][].

[gt-blog]: http://blog.gtmanfred.com/?p=26
[wiki]:    https://wiki.archlinux.org/index.php/Systemd/User
