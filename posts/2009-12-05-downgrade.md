---
title: Downgrade
tags: arch
---

Downgrade eases downgrading packages in Arch Linux.

## Examples

Downgrade some packages, checking both local cache and the A.R.M.:

```
$ downgrade foo bar
```

Downgrade a package, looking in only local cache:

```
$ NOARM=1 downgrade foo
```

Downgrade a package, looking in only the A.R.M.:

```
$ NOCACHE=1 downgrade foo
```

Downgrade a package, looking only in local cache, and favoring `su` over 
`sudo` even when `sudo` is available:

```
$ NOARM=1 NOSUDO=1 downgrade foo
```

## Installation

Install the AUR package [here][aur].

For more details, reporting Issues, etc, see the [GitHub project][repo].

[aur]: http://aur.archlinux.org/packages/downgrade
[repo]: http://github.com/pbrisbin/downgrade
