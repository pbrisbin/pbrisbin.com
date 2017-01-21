---
title: Selecting URLs via Keyboard in XTerm
tags: arch
---

In a recent effort to keep my latest laptop more standard and less customized,
I've been experimenting with XTerm over my usual choice of rxvt-unicode. XTerm
is installed with the `xorg` group, expected by the template `~/.xinitrc`, and
is the terminal opened by most window managers' default keybindings.

The only downside so far has been the inability to select and open URLs via the
keyboard. This is trivial to configure in `urxvt`, but seems impossible in
`xterm`. Last week, not having this became painful enough that I sat down to
address it.

<div class=well>
**UPDATE**: After a few weeks of use, discovering and attempting to fix a number
of edge-case issues, I've decided to stop playing whack-a-mole and just move
back to `urxvt`. Your mileage may vary, and if the setup described here works
for you that's great, but I can no longer fully endorse it.

I should've listened to [2009 me](https://bbs.archlinux.org/viewtopic.php?pid=629240#p629240).
</div>

## Step 1: `charClass`

Recent versions of XTerm allow you to set a `charClass` value which determines
what XTerm thinks are WORDs when doing a triple-click selection. If you do a bit
of [googling][triple-click], you'll find there's a way to set this `charClass`
such that it considers URLs as WORDs, so you can triple-click on a URL and it'll
select it and only it.

[triple-click]: https://lukas.zapletalovi.com/2013/07/hidden-gems-of-xterm.html#triple-click

[**~/.Xresources**][xresources]:

[xresources]: https://wiki.archlinux.org/index.php/X_resources

```
xterm*charClass: 33:48,37-38:48,45-47:48,64:48,58:48,126:48,61:48,63:48,43:48,35:48
```

I don't recommend trying to understand what this means.

## Step 2: `exec-formatted`

Now that we can triple-click-select URLs, we can leverage another feature of
modern XTerms, `exec-formatted`, to automatically send the selection to our
browser, instead of middle-click pasting it ourselves:

**~/.Xresources**:

```
*VT100.Translations: #override \n\
  Alt <Key>o: exec-formatted("chromium '%t'", PRIMARY) select-start() select-end()
```

## Step 3: `select-needle`

You might be satisfied there. You can triple-click a URL and hit a keybinding to
open it, no patching required. However, I despise the mouse, so we need to avoid
that triple-click.

Here's where `select-needle` comes in. It's a patch I found on the [Arch
forums][bbs] that allows you to, with a keybinding, select the first WORD that
includes some string, starting from the cursor or any current selection.

[bbs]: https://bbs.archlinux.org/viewtopic.php?id=181515

What this means is we can look for the first WORD containing "://" and select
it. You can hit the keybinding again to search up for the next WORD, or hit our
current `exec-formatted` keybinding to open it. Just like the functionality
present in `urxvt`.

I immediately found the patch didn't work in mutt, which is a deal breaker. It
seemed to rely on the URL being above `screen->cursorp` and mutt doesn't really
care about a cursor so it often leaves it at `(0, 0)`, well above any URLs on
screen. So I changed the algorithm to instead start at the bottom of the
terminal always, regardless of where the cursor is. So far this has been working
reliably.

I put the updated patch, along with a `PKGBUILD` for installing it on GitHub.
I'll eventually post it to the AUR to make this easier, but for now:

```
git clone https://github.com/pbrisbin/xterm-select-needle
(cd ./xterm-select-needle && makepkg -i)
rm -r ./xterm-select-needle
```

Then update that **~/.Xresources** entry to:

```
*VT100.Translations: #override \n\
  Alt <Key>u: select-needle("://") select-set(PRIMARY) \n\
  Alt <Key>o: exec-formatted("chromium '%t'", PRIMARY) select-start() select-end()
```

And that's it.
