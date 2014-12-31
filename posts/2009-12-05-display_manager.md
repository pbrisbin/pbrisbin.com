---
title: Display Manager
tags: arch, linux, bash
---

GDM, KDM, SLiM; they all serve one purpose: accept a username/password and start
X. The below accomplishes the same in the cleanest, simplest, most transparent
way I know.

```bash
# Note: a $SHELL of either bash or zsh is assumed

if [[ $TTY == /dev/tty1 ]] && [[ -z $DISPLAY ]]; then
  exec startx
fi
```

These are the last lines of my `~/.zprofile`, but they would work as well in
`~/.bashrc` if that's your preferred shell.

One added benefit here is that if X dies for any reason, you aren't left logged
in on `tty1` like you might be using some other display managers. This is since
the built-in `exec` replaces the current process with the one specified.
