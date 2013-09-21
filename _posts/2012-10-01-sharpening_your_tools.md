---
layout: post
title: "Sharpening Your Tools"
tags:
  - workflow
  - vim
---

Regardless of what editor you choose or how deeply you decide to extend 
or configure it, the most important thing for a professional programmer 
regarding his tool set is awareness.

You need to be acutely aware of situations where you do something 
repeatedly or slowly that you should automate or speed up. Quality 
editors like vim or emacs expose powerful constructs for doing tasks 
incredibly quickly. Ensuring you know all of these built-in constructs 
is the most common area where I see programmers (myself included) 
failing at this. It's too easy to fall into the habit of always using 
the slow way that might've been easiest to guess the first time you had 
to do something, or to install (or even write) some large plugin to give 
you a workaround to do the same thing the editor already provides 
(albeit by some obscure combination of constructs).

## Embrace Defaults

First of all, learn and try to embrace the default behavior of your 
editor. Try to lean on the side of less configuration, and absolutely 
never remap or override default behavior.

1. You'll feel comfortable on any system with that tool installed.
2. Your tool will be faster to use.
3. You'll find that many initially uncomfortable defaults actually make 
   a lot of sense, probably for reasons not immediately obvious to you.

That said, there's nothing wrong with creating additional behavior for 
(or extensions to) your tools to make the things you tend to do most 
often easier. So here are some vim tricks that I've recently added to my 
setup which you might find useful:

## Windows

I used to rely on my window manager to work with multiple windows, 
running multiple instances of my editor for each file I needed to work 
with at the same time. That was *horrible* of me. Not only does it use 
more resources than necessary, but it was also making moving code 
between files cumbersome and preventing me from exploring multi-file 
manipulation commands.

This also meant that when I moved to a system without a good window 
manager (like OS X), I began trying to fill that void with tools like 
tmux or iTerm splits. Guess what? Vim can split and manage windows 
just fine all by itself. Having all the open files in the same session 
also means I can `y` and `p` text between them, use commands like 
`:bufdo` or `:tabdo` to run commands over all open files, etc.

So get into the habit of using `:split` and `:vsplit` to open files you 
need to work on side by side. Use `:tabopen` to keep files open "in the 
background". And don't be afraid to `vim file1 file2` to get things open 
in multiple buffers at once.

When you do have multiple files open in panes of one buffer together, 
the following mapping will then make it easier to move around:

```
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l
```

Now you just `Ctrl-h` to focus left window, `Ctrl-j` to focus bottom, 
etc.

This is pretty standard stuff that I've had in my `vimrc` for ages, but 
recently, when watching a [destroy all software][das] screencast, I 
found something *amazing*:

[das]: http://destroyallsoftware.com

```
set winwidth=84
set winheight=5
set winminheight=5
set winheight=999
```

It's hard to describe what this does, but I'll try. When a pane is split 
vertically between two or more windows, the focused window will always 
have at least 84 columns, the other(s) will shrink to accommodate. This 
doesn't have much of an effect for me since my terminal usually has 
enough room to accommodate what I'm doing without squeezing anything in 
that direction, but what *is* amazing is what these settings do for 
horizontally split, vertically stacked windows.

Basically, all non focused windows shrink down to 5 lines and the 
focused window takes up everything that's left. This is an amazing 
workflow b/c it allows you to have a lot of files open at once, but they 
don't get in your way. You can still maintain focus on one at a time. 
Just try it, it's awesome.

## Stamp Out Annoyances

You have to always be on the lookout for things that you do repeatedly 
or unnecessarily. You should be deeply bothered by any action you're 
required to take that the computer could be doing for you.

For example, at least twice a day, I begin editing a file in a 
non-existent directory. This isn't just a mistake, I want that file at 
that path, I just don't care that the directories don't exist yet. It 
should be obvious to my tool that I want them created. 

```
function! Mkdir()
  let dir = expand('%:p:h')

  if !isdirectory(dir)
    call mkdir(dir, "p")
    echo "created non-existing directory: " . dir
  endif
endfunction

autocmd BufWritePre * call Mkdir()
```

Never again.

Another thing to watch out for is a simple and/or common action taking 
far too many steps or context switches. For example, I'm constantly 
creating a file at the wrong path. This happens most often when I'm 
adding a rails test for some controller a few directories deep. I'll 
forget to put the `_test` on the end of the file almost always.

Normally, I would have to save the file, remember that nested path, 
execute a `mv` command (and try to get it correct), then reopen the 
file.

```
" Based on https://github.com/km2r/vim-currentfile
function! Rename(dest)
  if &modified
    echoe "buffer is modified"
    return
  endif

  if len(glob(a:dest))
    echoe "destination already exists"
    return
  endif

  let filename = expand("%:p")
  let parent   = fnamemodify(a:dest, ":p:h")

  if !isdirectory(parent)
    call mkdir(parent, "p")
  endif

  exec "saveas " . a:dest
  call delete(filename)
endfunction

command! -nargs=1 -complete=file Rename call Rename(<f-args>)
```

With this, I just `:Rename to/what/it/was/supposed/to/be_test.rb` and I 
can continue editing straight away.

<div class="note">
Admittedly, I should just make an "add `_test` to the end of this file" 
mapping to fully satisfy my most common use case.
</div>

These were just a few examples of simple tweaks to speed up your editing 
flow, the important thing is to be very aware of the things your doing 
and not fall into bad habits. It's a fine line between bloat and 
convenience so be cognizant of that as well.

Most importantly, keep your tools sharp.
