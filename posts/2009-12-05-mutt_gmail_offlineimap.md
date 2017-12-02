---
title: Mutt + Gmail + Offlineimap
tags: linux, mutt
---

Most people use Gmail. Some people like CLI mail clients. This post
describes how I use Gmail in the best CLI mail client, mutt. Many people
will back me up when I say it's a very good setup.

For reference, my complete and current setup can be found with my 
[dotfiles][].

[dotfiles]: https://github.com/pbrisbin/dotfiles/tree/v1.0/tag-mail-recipient

## Offlineimap

Step one is to setup Offlineimap to keep `~/Mail` in sync with Gmail.
This is a two way sync so anything moved, deleted, or sent from any
IMAP-connected device or our local mutt interface will act exactly
the same. This also has the benefit of storing offline, local
copies of all your mails.

First, install Offlineimap and fill in an `~/.offlineimaprc` like so:

```python
[general]
ui = ttyui
accounts = Gmail

[Account Gmail]
localrepository = Gmail-Local
remoterepository = Gmail-Remote

[Repository Gmail-Local]
type = Maildir
localfolders = ~/Mail/Gmail

[Repository Gmail-Remote]
type = Gmail
remoteuser = you@gmail.com
remotepass = secret
realdelete = no
maxconnections = 3
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
```

Test that this works by running `offlineimap -o`. Your first sync could
take some time, but once done, you should see the folders under
`~/Mail/Gmail` with the proper structure.

Once you're satisfied syncing is working, we'll schedule a periodic sync 
via cron.

<div class="well">
There are some tempting options offlineimap has for daemonizing itself 
to handle periodic syncing for you -- in my experience these don't work. 
Scheduling a full offlineimap run via cron is the only working setup 
I've been able to find.
</div>

To work around a thread-joining bug, I've landed on a wrapper script 
that spawns offlineimap to the background then babysits the process for 
up to 60 seconds. If it appears to be hung, it's killed.

```bash 
#!/usr/bin/env bash

# Check every ten seconds if the process identified as $1 is still 
# running. After 5 checks (~60 seconds), kill it. Return non-zero to 
# indicate something was killed.
monitor() {
  local pid=$1 i=0

  while ps $pid &>/dev/null; do
    if (( i++ > 5 )); then
      echo "Max checks reached. Sending SIGKILL to ${pid}..." >&2
      kill -9 $pid; return 1
    fi

    sleep 10
  done

  return 0
}

read -r pid < ~/.offlineimap/pid

if ps $pid &>/dev/null; then
  echo "Process $pid already running. Exiting..." >&2
  exit 1
fi

offlineimap -o -u quiet & monitor $!
```

Set this script to run as frequently as you want, by adding something 
like the following to your crontab -- I chose to sync once every 3 
minutes:

```
*/3 * * * * /path/to/mailrun.sh
```

## Msmtp

Now we need a way to send mails. I like msmtp, you can also use other
smtp clients. If you choose to install msmtp, the config file is at
`~/.msmtprc` and should look like this:

```
account default 
host smtp.gmail.com
port 587
protocol smtp
auth on
from user@gmail.com
user user@gmail.com
password secret
tls on
tls_nocertcheck
```

You can test this by executing `echo "a test message" | msmtp
you@gmail.com`.

## Mutt

Now the fun part! I don't know how many hours I've spent in the past
year fine tuning my muttrc, but it'll never be done. Here are the parts
required to get this setup working.

```
set mbox_type   = Maildir
set sendmail    = /usr/bin/msmtp

set folder      = ~/Mail
set spoolfile   = "+INBOX"
set mbox        = "+[Gmail]/All Mail"
set postponed   = "+[Gmail]/Drafts"
unset record

mailboxes +INBOX

macro index D \
    "<save-message>+[Gmail]/Trash<enter>" \
    "move message to the trash"

macro index S \
    "<save-message>+[Gmail]/Spam<enter>" \
    "mark message as spam"
```

The above should be enough to get a connection and start 
sending/receiving mail, but here are some other must-have options that 
make it feel a bit more like gmail:

```
# main options
set realname   = "Real Name"
set from       = "user@gmail.com"
set mail_check = 0
set envelope_from

unset move           # gmail does that
set delete           # don't ask, just do
unset confirmappend  # don't ask, just do!
set quit             # don't ask, just do!!
unset mark_old       # read/new is good enough for me

# sort/threading
set sort     = threads
set sort_aux = reverse-last-date-received
set sort_re

# look and feel
set pager_index_lines = 8
set pager_context     = 5
set pager_stop
set menu_scroll
set smart_wrap
set tilde
unset markers

# composing 
set fcc_attach
unset mime_forward
set forward_format = "Fwd: %s"
set include
set forward_quote

ignore *                               # first, ignore all headers
unignore from: to: cc: date: subject:  # then, show only these
hdr_order from: to: cc: date: subject: # and in this order
```

I've left out quite a few tweaks in the above so that those who are
happy with mutt's *very sane* defaults aren't overwhelmed. Keep in mind,
`man muttrc` is a great command for when you're bored.

That should do it. Hopefully this info will get you going in the right
direction.
