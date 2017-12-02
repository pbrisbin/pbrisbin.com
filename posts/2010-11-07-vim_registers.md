---
title: Vim Registers
tags: vim
---

When you use an extremely powerful text editor such as vi, vim, or 
emacs, there are often times where you'll discover a feature or command 
that literally changes the way you write text. It's not a very large 
leap to say that, for a developer, that can be *life-changing*.

I've recently made one such discovery via vim's `:help registers` 
command. So I'd like to boil it down a bit and share it here.

## Pasting in Vim

Often times when idling in `#archlinux`, someone will ask about pasting 
in vim.

Answers typically range from `:set paste`, to `S-<insert>`, etc, but one 
staple response is `"*p` and `"+p`.

These commands will take the contents of your X11 selection (currently 
highlighted text) and clipboard (text copied with `C-c`) respectively 
and dump it into your buffer.

I've heard these commands several times but I could never remember them. 
The reason is because I didn't really know what they did. I mean, 
obviously I knew that they pasted into vim from said locations, but I 
didn't know what those three command characters meant. Today, I decided 
to find out.

## Registers in Vim

Vim has a number of what's called *registers*, they're just dumping 
grounds for text. Vim uses these to store different snippets of text for 
different reasons in very auto-magical ways. For instance, this is how 
`undo` is implemented in vim.

If you understand how vim is storing this text and how to read and write 
from these registers yourself, it can really help your work flow.

Here's the list reproduced from `:help registers`:

1. The unnamed register `""`
2. 10 numbered registers `"0` to `"9`
3. The small delete register `"-`
4. 26 named registers `"a` to `"z` or `"A` to `"Z`
5. four read-only registers `":`, `".`, `"%` and `"#`
6. the expression register `"=`
7. The selection and drop registers `"*, `"+ and `"~`
8. The black hole register `"_`
9. Last search pattern register `"/ `

Editing commands (think `d`, `y`, and `p`) can be prefixed with a 
register to tell vim where to read or write the text you're working 
with.

The unnamed register is the default and holds the most recently deleted 
or yanked text; it's what's called upon when you just type `p` without 
specifying a register.

Now, have you ever `dd`ed something, `dd`ed something else, but then 
realized you really want to `p` that first thing you deleted?

Up until now, I would `u` back two steps and re-order my deletes so the 
text I wanted to `p` was the one most recently `dd`ed.

I should've known that vim had a much more powerful way to deal with 
this. Registers 0 through 9 hold that list of deleted text. In my case I 
could've simply done `"1p` to put not the most recently `dd`ed text 
(which is `"0p`, `""p`, or just `p`), but the text one step before that.

The 26 named registers are meant to be used purposely by you to store 
snippets as you work. Calling them as `a` vs `A` simply means replace or 
append.

Ever wonder *how* the `.` command actually works in vim? Yeah me either. 
Anyway, it's just the read-only register `".` that holds your most 
recent action. Typing `.` just tells vim to call it up and execute it.

And finally, the explanation for `"*p` and `"+p`, the selection and drop 
registers. They work just like any other and store the contents of the 
X11 selection and clipboard. That way, calling `"*p` simply dumps the 
register into your buffer.

What's more, you can use `Ctrl-v` to highlight a visual block, then type 
`"+y` to put that text into your clipboard to go paste it somewhere.

Another neat trick is the last search pattern. You can actually write to 
that register with what's known as a `let-@` command. That way, if 
you're using `hlsearch`, you can tell vim to highlight words without 
actually searching for them (and possibly moving your cursor).

    :let @/ = "the"

I'll let you `:help` yourself regarding the other registers.
