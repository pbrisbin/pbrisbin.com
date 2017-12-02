---
title: Developing In OS X
tags: osx
---

As everyone who happens upon this site probably knows, I prefer to 
develop software in linux. The toolset is just better. Having and being 
proficient with a good shell is an invaluable tool for working with 
files. And regardless of what windows-ey, gui-IDE-ey developers like to 
say -- software development is working with plain text files.

My work computer is now a Macbook. It's about a million steps in the 
right direction from my last work-provided computer, but it's still not 
linux.

That said, it's damn close. It's a unix variant originally based on BSD, 
it's got a good shell and just about every linux tool I've grown 
accustomed to can be easily and quickly installed and utilized here.

So, this post is intended to describe the things I've installed and 
configured to get my development environment the way I like it on this 
platform.

## The Terminal

It all starts with the terminal... and Terminal.app ain't it. For a long 
time, I used iTerm simply because it supported 256 colors which no other 
Mac terminal does.

It was recently that I noticed there was a general lag when scrolling 
line by line in commandline-vim inside iTerm. This was unacceptable and 
prompted me to try working in MacVim for some time.

MacVim was fine and all, but then I found there was an iTerm2. There's 
no lag in the newer terminal version, the preferences pane seems more 
thought out, and it's just generally better. So go out and download 
[iTerm2][iterm] as your terminal-of-choice on the Mac.

[iterm]: http://www.iterm2.com/#/section/home

## The Multiplexer

A terminal multiplexer offers a number of benefits. Of these, the 
biggest ones in my opinion are:

1. Detach and reattach sessions

If you work in a multiplexer, your terminal never closes. All of your 
work is bundled up in this workspace-terminal that's running inside and 
on top of your real terminal. If your ssh connection dies, your terminal 
crashes, or you actively "detach", your work is still sitting there in 
that workspace. You can pull it up and reattach it to some other 
terminal whenever you want.

You can also have multiple named sessions which you can detach and 
reattach to shift gears or just stay generally organized.

2. Split into regions

In linux I have a great tiling window manager. My desktop can be neatly 
split into multiple terminals where I can spread out my work.

I don't really have that on the Mac. I tried for a while to get a good 
WM going in X11, but it just never clicked. So as an alternative, I can 
use a multiplexer to split one full-screen iTerm instance into any 
number of tabs, and/or vertical and horizontal regions.

I typically leave one half-term column for `vim` (which itself can be 
split any number of ways) then use the other side for running a `tail 
-f` on the log, a `mysql` console, and possibly `autotest` or `watchr`.

3. Keyboard driven navigation

Navigation between regions, copy/paste, and everything else is completed 
by fully configurable key bindings. Not needing to reach for the mouse 
is a huge productivity win for me.

So, what multiplexer?

Well, in my opinion `screen` does 1 and 3 great. It's what I use and 
will always use on linux -- when I have the WM to do the 
screen-splitting.

However, `tmux` owns in the screen-splitting department. So on the Mac, 
I recommend `tmux`. Google around for a good `tmux.conf` and spend some 
time with the manpage; you won't regret it.

## The Editor

In my opinion there is no alternative to a good `vim` setup. Luckily, it 
works just fine on the Mac. In fact, my [vim-config][] worked without 
any modifications whatsoever.

[vim-config]: https://github.com/pbrisbin/vim-config

If anyone's interested, here are the plugins I currently roll with:

```
ls ~/.vim/bundle
additional-surroundings
command-t
haskellmode
hoogle
nerdcommenter
simplefold
supertab
surround
vim-endwise
vim-fugitive
vim-git
vim-rails
vim-ruby
```

And If you're not using `pathogen`, get to [googlin][].

[googlin]: http://www.google.com/search?gcx=w&sourceid=chrome&ie=UTF-8&q=use+pathogen+and+git+submodules+for+vim+plugins

## The Other Stuff

Pretty much any unix commandline utility can be installed via ports or 
homebrew. I recommend grabbing GNU coreutils so you've got a better `ls` 
and friends. bash-completion and proctools are two others that will make 
things feel a bit more linux-ey.

Also do yourself a favor and [upgrade][] bash to 4.0. It comes with 
`globstar` which itself is more than worth it.

[upgrade]: http://concisionandconcinnity.blogspot.com/2009/03/upgrade-bash-to-40-in-mac-os-x.html

## The Bottom Line

Learn to live in a terminal -- use an editor and utilities that fit 
there. Use a multiplexer like `tmux` or `screen` in a quality terminal 
like iTerm2.
