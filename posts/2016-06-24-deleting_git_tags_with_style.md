---
title: Deleting Git Tags with Style
tags: git
---

Deleting Git tags that have already been pushed to your remote is something I
have to google literally every time I do it; why the invocation is so arcane, I
don't know. Finally, I decided to automate it with a custom sub-command:

**~/.local/bin/git-delete-tag**

```bash
#!/bin/sh
for tag; do
  git tag -d "$tag"
  git push origin :refs/tags/"$tag"
done
```

With this script present on `$PATH`, I can just invoke `git delete-tag TAG,
...`. This is great, but I soon noticed that typing `git dele<tab>` wouldn't
complete this command (or any custom sub-commands for that matter). After a
little digging in the `_git` completion file, I found the relevant `zstyle`
needed to get this working:

**.zshrc**

```bash
zstyle ':completion:*:*:hub:*' user-commands ${${(M)${(k)commands}:#git-*}/git-/}
```

Since I'm actually invoking [`hub`][hub], a `git` wrapper with added
functionality for interacting with GitHub, I had to use `:hub:` in place of
`:git:`, which is what the documentation shows.

[hub]: https://github.com/github/hub

I also wanted `git delete-tag <tab>` to complete with the current tags for the
repository. Again, the extension points in the Zsh tab-completion system shine,
and it only took a little `_git-` completion function to make it happen:

**.zshrc**

```sh
_git-delete-tag() { compadd "$@" $(git tag) }
```

Hopefully this short post will come in useful for Git and Zsh users who, like
myself, can never remember how to delete Git tags. As always, you can find the
described configuration "in the wild" by way of my [dotfiles repo][]. These
items will be within the `scripts` or `zsh` tags.

[dotfiles repo]: https://github.com/pbrisbin/dotfiles/tree/v1.0
