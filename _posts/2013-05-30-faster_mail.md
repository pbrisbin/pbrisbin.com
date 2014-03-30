---
layout: post
title: "Faster Mail"
tags:
  - mutt
  - email
  - offlineimap
  - system
---

I hear and see a lot of passing complaints about dealing with a large 
amount of mail. I myself subscribe to a few mailing lists which get 
quite a bit of traffic and these are usually the first to be ignored 
when I get behind. Once a backlog of unread mail piles up it can be hard 
to get any traction. The sad part is I enjoy that content, so I 
definitely don't want to be missing out just because I occasionally 
can't keep up.

As readers of this site might know, I use mutt to interact with my mail. 
Well, I recently implemented some subtle changes which have allowed me 
to get through this mail more quickly and not stop altogether just 
because I get a bit behind.

I thought it might be useful to outline these tips for others. 
Obviously, it assumes a similar setup as mine and some of these might 
not translate very well.

## Choose what you care about

I used to sync everything down from Gmail into the local Maildir 
assuming I'd want the option to search it. Turns out, I almost never do, 
in stead these mailboxes just clutter up any view of "what's new" that 
mutt is trying to give me.

Assuming you're using my [python helper][] for offlineimap, you can very 
easily exclude some of the noise from being synced:

[python helper]: https://github.com/pbrisbin/dotfiles/blob/master/tag-mail-recipient/offlineimap.py

**~/.offlineimaprc**

```
[general]
pythonfile = ~/.offlineimap.py

[Repository Gmail-Remote]
folderfilter = exclude([ '[Gmail]/All Mail'
                       , '[Gmail]/Important'
                       , '[Gmail]/Spam'
                       , '[Gmail]/Starred'
                       , 'Priority'
                       ])
```

<div class="well">
Yes, I exclude the "Important" stuff. How ironical.
</div>

You can also tell offlineimap to write a mailboxes file for mutt to 
source. This way, mutt will stay up to date on exactly what folders you 
care about and you only need to declare what you (don't) care about in 
one place.

**~/.offlineimaprc**

```
[mbnames]
enabled = yes
filename = ~/.mutt/mailboxes
header = "mailboxes "
peritem = "+%(accountname)s/%(foldername)s"
sep = " "
footer = "\n"
```

**~/.mutt/muttrc**

```
source ~/.mutt/mailboxes
```

## Mailboxes that matter

Now that you've got mutt aware only of mailboxes that matter, you'll 
find that pressing `c` to change mailboxes automatically becomes more 
useful.

Mutt auto-fills the Change-to prompt with the mailbox which last 
received new mail. Assuming you're actually keeping things clean (which 
we'll get to soon), that's the mailbox you should be looking at.

`c Enter`, Deal with it. `c Enter`, Deal with it...

When you find an empty prompt, you're done.

## Don't leave until you deal

So now that you can move quickly through any and all mailboxes that 
require your attention, how do you actually deal with it?

First, I setup my mailbox to show all threads as collapsed, with a 
keybind to toggle them open:

**~/.mutt/muttrc**

```
folder-hook * "exec collapse-all"

macro index ,v "<collapse-thread>" "collapse/uncollapse thread"
macro index ,V "<collapse-all>"    "collapse/uncollapse all threads"
```

I also color threads with new mails (and the new mails themselves):

```
# threads containing new messages
uncolor index "~(~N)"
  color index brightblue default "~(~N)"

# new messages themselves
uncolor index "~N"
  color index brightyellow default "~N"
```

Now I have a good overview (at the mailbox level) of what I'm dealing 
with. From there I can expand-read what I want, mark whole threads as 
read, or mark the whole mailbox as read.

Assuming the majority of threads are closed in this mailbox, I use the 
following to mark-as-read only open threads:

```
macro index ,r \
  "<tag-pattern>all<return><tag-prefix><clear-flag>N<untag-pattern>all<return>" \
  "mark all as read"
```

This macro is technically *mark all read*, but in actuality only open 
threads are affected. It also catches any one-message threads as they're 
by-definition open as well. I expect and rely on this behavior, but I 
understand if it's not for everyone. 

In the cases that I do want to mark the whole mailbox read including any 
closed threads, I just use this second macro to open everything before 
running the above, then close it up again after:

```
macro index ,R "<collapse-all>,r<collapse-all>" "mark all as read (collapsed)"
```

That just about sums it up. The best part, if I'm feeling particularly 
unreachable, is to blindly do the following:

`c Enter , R c Enter , R`... Until it's All Gone.
