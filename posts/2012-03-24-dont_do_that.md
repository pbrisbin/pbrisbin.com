---
title: Dont Do That
tags: arch, linux, pacman
---

I use Arch linux for a number of reasons. Mainly, it's transparent and 
doesn't hold your hand. You're given simple, powerful tools and along 
with that comes the ability to shoot yourself in the foot. This extends 
to the community where we can and should help those newer than ourselves 
to manage this responsibility intelligently, but without holding their 
hand or taking any of that power away through obfuscation.

## The Problem

There's always been the potential for a particular command to break your 
system:

```
$ pacman -Sy foo
```

What this command *literally* means is, "update the local index of 
available packages and install the package `foo`". Misguided users 
assume this is the correct way to ensure you receive the latest version 
of `foo` available. While it's true that it is *one* way, it's not the 
*correct* way. Moreover, using this command can easily break your 
system.

Let's walk through an example to illustrate the problem:

* A user has firefox 3.0 and gimp 6.1 installed, both of which depend on 
  libpng>=1.0
* An update comes out for libpng to version 1.2
* Arch maintainers release libpng 1.2, firefox 3.0-2, and gimp 6.1-2 
  (the latter two now depending on libpng>=1.2)
* An update comes out for firefox to version 3.1
* Arch maintainers release firefox 3.1 which depends on libpng>=1.2
* Our user (incorrectly) says `pacman -Sy firefox` hoping to get this 
  new version
* pacman (correctly) installs firefox 3.1 and libpng 1.2

There's nothing here to tell pacman to update gimp since libpng 1.2 is 
>= 1.0 which meets gimp's dependency contstraints.

However, our user's gimp binary is actually linked directly to 
/usr/lib/libpng.so.1.0 and is now broken. Sadface.

In this example, the outcome is a broken gimp. However, if the shared 
dependency were in stead something like readline and the broken package 
something like bash, you might be left with an unusable system requiring 
a rescue disk or reinstall. This of course lead to a lot of unhappy 
users.

## The Solution

There are a few options to avoid this, the two most viable being:

1. Instruct users to not execute `-Sy foo` unless they know how `foo` 
   and its dependencies will affect their system.
2. Instruct Arch maintainers to use a hard constraint in these cases, so 
   firefox and gimp should depend on libpng==1.0

If we went with option two, the user, upon running `pacman -Sy firefox` 
would've gotten an error for unresolvable dependencies stating that gimp 
requires libpng==1.0.

Going this route might seem attractive (especially to users) but it 
causes a number of repository management headaches dealing with exact 
version constraints on so many heavily depended-upon packages. The 
potential headache to the maintainers far out-weighed the level of 
effort required to educate users on the pitfalls of `-Sy`.

So, option one it is.

## The Wrong Advice

It was decided (using the term loosely) to tell anyone and everyone to 
always, no matter what, when they want to install `foo`, execute:

```
$ pacman -Syu foo
```

I argue that this advice is so opposite to The Arch Way, that it's 
downright evil.

What this command really says is, "update your system and install 
`foo`". Sure, that's no big deal, it's not harmful, may or may not be 
quick and ensures you don't run into the trouble we've just described.

Coincidentally, this is also the correct way to ensure you get the 
absolute latest version of `foo` -- if and only if `foo` had a new 
version released since your last system update.

My issue is not that it doesn't work. My issue is not that it's 
incorrect advice to those with that specific intention. My issue is 
that, nine times out of ten, that's not the user's intention. They 
simply want to install `foo`.

You're now telling someone to run a command that does more than what 
they intended. It does more than is required. It's often given out as 
advice with no explanation and no caveats. "Oh, you want to install 
something? `-Syu foo` is how you do that..." No, it really isn't.

You've now wasted network resources, computational resources, the user's 
time and you've taught them that the command to install `foo` is `-Syu 
foo`. Simplicity and transparency aside, that's just **lying**.

If you've been given this advice, I'm sorry. You've been done a 
disservice.

## The Correct Advice

To update your system:

```
$ pacman -Syu
```

To install `foo`:

```
$ pacman -S foo
```

To update your system *and* install `foo`:

```
$ pacman -Syu foo
```

Simple, transparent, no breakage. That's the advice you give out.

Sure, by all means, if your true intention is to upgrade the system and 
install `foo`, you should absolutely `-Syu foo` but then, and only then, 
does that command make any sense.

`</rant>`
