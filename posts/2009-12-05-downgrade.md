---
title: Downgrade
tags: aur, arch, linux, bash
---

*A special thanks to Kumyco for hosting the A.R.M.*

## Usage

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

Grab the source from my git repo [here][repo].

[aur]: http://aur.archlinux.org/packages/downgrade
[repo]: http://github.com/pbrisbin/downgrade
