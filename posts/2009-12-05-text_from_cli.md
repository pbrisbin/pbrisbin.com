---
title: Text From CLI
tags: linux
---

This is a short but extensible script to allow text messaging (to
verizon customers) straight from the commandline.

Setup requires simply a means to send email from the commandline
along with a small script to pass the message off to
`<number>@vtext.com`.

If you already have a CLI mailing solution you can just copy the
script and go ahead and change the mail command to mutt, ssmtp,
mailx, or whatever you're using.

## Email from CLI

I use msmtp to send mails in mutt so it was easy for me to adapt
that into a CLI mailing solution.

Here's a `~/.msmtprc` for gmail:

```
# msmtp config file

# gmail
account gmail
host smtp.gmail.com
port 587
protocol smtp
auth on
from username@gmail.com
user username@gmail.com
password gmail_password
tls on
tls_nocertcheck

account default : gmail
```

Right now, as-is, it's possible for you to
`echo "Some text" | msmtp someone@somewhere.com` and it'll email
just fine. I'd like to make things a little more flexible.

By dropping a file in `~/.mailrc` we can change the `mail` command
to use whatever binary we want instead of the default
`/usr/bin/sendmail`. It should have the following contents:

    set sendmail=/usr/bin/msmtp

Now, anytime your system mails anything on your behalf, it'll use
`msmtp`.

## The Script

The script started out very simply, here it is in its original form:

```bash 
#!/bin/bash

if [[ $# -lt 2 ]]; then
  echo "usage: $0 [number] [some message]"
  exit 1
fi

number="$1"; shift

echo "$*" | mail "$number@vtext.com"
```

With this little sendtext.sh script in your back pocket, you can
send yourself texts from remind, cron, rtorrent, or any other
script to notify you (or other people) of whatever you want.

    sendtext.sh 1234567890 'This is a test text, did it work?'

Sure did.

Now, at some point, [Ghost1227](http://ghost1227.com) got bored
again.

He took my sendtext script and ran with it. Added loads of carriers
and some new option handling.

I took his update of my script and re-updated it myself. Mainly
syntactical changes and minor options handling, just to tailor it
to my needs.

The new version with my and ghost's changes can be downloaded from
my
[git repo](http://github.com/pbrisbin/scripts/blob/pre-cleanout/sendtext).

I also added simple phone book support. When sending a message to
someone, pass `-s <number> <name>` and the contact will be saved to
a text file. After that, you can just `sendtext <name>` and the
most recent match out of this text file will be used. The service
is saved as well (either the default or the one passed as an
argument at the time of -s).
