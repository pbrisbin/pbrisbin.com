---
title: Screen Tricks
tags: linux
---

Hopefully, if you're a CLI junky, you've heard of GNU/[screen][]. And if 
you've heard of it, chances are you're using it.

Screen is a terminal multiplexer. This means that you can start screen 
in one terminal (say, your SSH connection) and open any number of 
terminals *inside* that terminal. This lets me have mutt, ncmpcpp, and a 
couple of spare shells all open inside my single PuTTY window at work.

This is a great use of screen, but the benefits don't have to end there. 
When I'm not at work but at home, I can use screen to run applications 
which I don't want to end if I want to change terminals, log in and out, 
or even if all of X comes crashing down around me.

See, screen can *detach* (default binding: C-a d). Better still, It will 
auto-detach if the terminal it's in crashes or you logout. You can then 
re-attach it later, from any other ssh session, tty, or X terminal.

This is great for apps like rtorrent and irssi, it's also great for not 
losing any work if your ssh connection gets flaky. Just re-connect and 
re-attach.

So now I have a dilemma. When I'm at work, I want to start screen and 
get a few fresh tabs set up as I've defined in `~/.screenrc`: mutt, 
ncmpcpp, and three shells. But at home I *don't* want those things to 
load, I instead want *only* rtorrent or *only* irssi to load up in the 
new screen window.

Furthermore, if rtorrent or irssi are already running in some detached 
screen somewhere, I don't want to create an entirely new session, I'd 
rather grab that one and re-attach it here.

The goal was to achieve this without changing the commands I run day to 
day, affecting any current keybinds, or using any overly complicated 
scripts.

So, how do I do this as simply and easily as possible? Environment 
variables.

## How to do it

First we set up one main `~/.screenrc` which is always called. Then we 
set up a series of "screenrc extensions" which *only* load the apps in 
the screen session via a stanza of `screen -t <name> <command>` lines.

Next, we dynamically choose which "screenrc extension" to source from 
the main `~/.screenrc` via two environment variables which are either 
exported from `~/.bashrc` (the default) or explicitly set when running 
the command (the specialized cases).

So, set up a `~/.screenrc` like this:

```bash 
# screen config file; ~/.screenrc

# put all our main screen settings like
# term, shell, vbell, hardstatus whatever
#
# then add this:

# sources environment-specific apps
source "$SCREEN_CONF_DIR/$SCREEN_CONF"

# you can even add some tabs you'll always
# open no matter what

# then always open some terms
screen -t bash $SHELL
screen -t bash $SHELL
screen -t bash $SHELL
```

Now, how does screen know what "screenrc extension" to source? By 
setting those variables up in `~/.bashrc`:

```bash 
# dynamically choose which tabs load in screen
export SCREEN_CONF_DIR="$HOME/.screen/configs"
export SCREEN_CONF="main"
```

In a clean environment, screen will source that default 
`~/.screen/configs/main`, which will:

```bash 
# example: screen -t [name] [command]
screen -t mail mutt
screen -t music ncmpcpp
```

Why is this useful? Because, now I can do something like this:

    SCREEN_CONF=rtorrent screen

And screen will instead source that explicitly set 
`~/.screen/configs/rtorrent` which yields:

```bash 
# example: screen -t [name] [command]
screen -t torrents rtorrent 
```

Et viola, no mutt or ncmpcpp, but rtorrent instead (same thing happens 
with irssi).

Oh, but it gets better! Now we'll add some aliases to `~/.bashrc` to 
complete the whole thing:

```bash 
alias irssi='SCREEN_CONF=irssi screen -S irssi -D -R irssi'
alias rtorrent='SCREEN_CONF=rtorrent screen -S rtorrent -D -R rtorrent'
```

Oh how beautiful, how simple, how easy. I type `rtorrent`, what happens?

Screen checks for any running screens with session-name "rtorrent" and 
re-attaches here and now. If none are found, screen opens a new screen 
(using the rtorrent file) and names the session "rtorrent" so we can -D 
-R it explicitly thereafter.

All of this happens for `irssi` too, and can be used for any app (or 
multi-app setup) you want.

Pretty KISS if I do say so.

[screen]: http://www.gnu.org/software/screen/ "screen at gnu.org"
