---
title: Automounting
tags: linux
---

It seems as users (myself inclusive) progress through the stages of 
using a distribution like Arch linux, they reach certain stages.  Like 
when you realize how amazing `find -exec` is. Or crossing over from 
*god, vim is a pain in the ass!* to *jesus, why doesn't everyone use 
this?*

I find one well-known stage is *how can I automount my USB drives?* This 
usually comes early on as a new Arch user ditches GNOME or KDE in favor 
of something lighter, something more minimalistic, something they can 
actually be proud to show off in the screenshot thread. Well, ditch the 
DE and you lose all those nifty little automagical tools, like 
gnome-volume-manager and the like.

So what do you do? hal *should* take care of it. Some 
`ck-launch-session` black magic might do the trick. Edit some \*.fdi 
file to get it going?

No. Udev does just fine.

## Udev

Udev has a little folder called `/etc/udev/rules.d`. In this folder, are 
'rules files' each named `10-some-crap.rules`. They are processed one by 
one each time some udev 'event' occurs, like, say, plugging in a 
flashdrive.

<div class="well">
Go google udev rules, there's a lot out there for all sorts of nifty 
things.
</div>

Someone smarter than I added a handful of useful rules to the Arch [udev 
wiki page][]. The one I use is as follows:

    # adjust this line to skip any persistent drives
    # i.e. KERNEL!="sd[d-z][0-9]", ...
    KERNEL!="sd[a-z][0-9]", GOTO="media_by_label_auto_mount_end"

    # Global mount options
    ACTION=="add", ENV{mount_options}="relatime,users"

    # Filesystem specific options
    ACTION=="add", PROGRAM=="/lib/initcpio/udev/vol_id -t %N", RESULT=="vfat|ntfs", ENV{mount_options}="$env{mount_options},utf8,gid=100,umask=002"
    ACTION=="add", PROGRAM=="/lib/initcpio/udev/vol_id --label %N", ENV{dir_name}="%c"
    ACTION=="add", PROGRAM!="/lib/initcpio/udev/vol_id --label %N", ENV{dir_name}="usbhd-%k"
    ACTION=="add", RUN+="/bin/mkdir -p /media/%E{dir_name}", RUN+="/bin/mount -o $env{mount_options} /dev/%k /media/%E{dir_name}"
    ACTION=="remove", ENV{dir_name}=="?*", RUN+="/bin/umount -l /media/%E{dir_name}", RUN+="/bin/rmdir /media/%E{dir_name}"
    LABEL="media_by_label_auto_mount_end"

This file defines how udev reacts to usb drives (`/dev/sda1`, etc) being 
added and removed. You plug in a flashdrive, if it has a label, it's 
mounted at `/media/<label>`; if not, it's mounted at `/media/usbhd_sda1` 
(for example). `umount` and remove the drive, and that directory under 
`/media` is removed. It's a beautiful thing.

## Automount

One problem I found with this is that it works really well. When a 
device is added it is mounted, period. So whenever I tried to partition 
a drive, *as soon as the partition was initialized* it would get 
mounted, and the partitioning tool would fail with `drive is mounted`.

For this reason, I had to write a [script][]. I always have to write a 
script.

What this does is simply write the above rules file or remove it.  This 
effectively turns automounting *on* or *off*. So there you go, simple 
handling of usb flash drive with nothing but udev required.

## DVDs and CDs

Just a bit about optical media. The above won't solve any issues related 
to that. I'll just say this though, if I need to do anything related to 
CDs or DVDs, I can just reference `/dev/sr0` directly. Burning images, 
playing DVDs, it all works just fine using `/dev` directly. And when I 
need to mount it, I'll do it manually. I think a line in `fstab` will 
get `/dev/sr0` to mount to `/media/dvd` if that's what your after.

[script]: http://github.com/pbrisbin/scripts/blob/pre-cleanout/automount "automount on github"
[udev wiki page]: http://wiki.archlinux.org/index.php/Udev "arch wiki"
