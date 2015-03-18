---
title: Using Two IMAP Accounts in Mutt
tags: linux, gmail, mutt
---

Mutt can be really great with multiple accounts, but it's not exactly
intuitive to setup. Here I'll document how I access two Gmail accounts 
together in one mutt instance.

<div class="well">
If you haven't yet seen my previous mutt post, please go read [that][] 
now. I recommend using that post to get a single account setup first 
before coming back here. Even if you plan to jump right into a 
multi-account setup, this post assumes you've at least read the other 
one and will focus on the differences and required changes to get from 
there to here.
</div>

[that]: https://pbrisbin.com/posts/mutt_gmail_offlineimap

## Offlineimap

To get Offlineimap syncing multiple accounts, we simply need to add 
additional configuration blocks to sync the second account with another 
local Maildir.

**~/.offlineimaprc**

```python
[general]
ui = ttyui
accounts = Personal,Work

[Account Personal]
localrepository = Personal-Local
remoterepository = Personal-Remote

[Account Work]
localrepository = Work-Local
remoterepository = Work-Remote

[Repository Personal-Local]
type = Maildir
localfolders = ~/Mail/Personal

[Repository Work-Local]
type = Maildir
localfolders = ~/Mail/Work

[Repository Personal-Remote]
type = Gmail
remoteuser = username@gmail.com
remotepass = secret
realdelete = no
sslcacertfile = /etc/ssl/certs/ca-certificates.crt

[Repository Work-Remote]
type = Gmail
remoteuser = work-username@gmail.com
remotepass = secret
realdelete = no
sslcacertfile = /etc/ssl/certs/ca-certificates.crt
```

<div class="well">
Obviously, if either of these accounts weren't a Gmail server, the 
configuration blocks would be different.
</div>

You can test your setup by running `offlineimap -o` to sync things once.
It could take a while, but once done, you should have a nice folder
structure like this:

    Mail/
    |-- Personal
    |   |-- INBOX
    |   `-- ...
    `-- Work
        |-- INBOX
        `-- ...

## Msmtp

Msmtp also handles multiple accounts very elegantly, we just add another 
account block for the second account.

**~/.msmtprc**

```
# shared defaults since both are gmail accounts
defaults
host smtp.gmail.com
port 587
protocol smtp
auth on
tls on
tls_trust_file /etc/ssl/certs/ca-certificates.crt

account personal
from username@gmail.com
user username@gmail.com
password secret

account work
from work-username@gmail.com
user work-username@gmail.com
password secret

account default : personal
```

Now we can simply call `msmtp -a personal` or `msmtp -a work` to use
whichever account we want. Omitting the `-a` option will use the default 
account which we've set as personal.

## Mutt

The goal with mutt is to have certain settings change when we enter 
certain folders. For example, when we're viewing `+Personal/INBOX` we 
want our `from` setting to be our personal From address and the 
`sendmail` setting should be `msmtp -a personal`. To provide this 
functionality, we're going to do the following:

1. Place any account-specific settings in separate files
2. Use mutt's `folder-hook` facility to source the proper file and set 
   the proper settings upon entering a folder for a given account.

Here are the two account-specific files:

**~/.mutt/accounts/personal**

```
set from      = "username@gmail.com"
set sendmail  = "/usr/bin/msmtp -a personal"
set mbox      = "+Personal/archive"
set postponed = "+Personal/drafts"

color status green default

macro index D \
    "<save-message>+Personal/Trash<enter>" \
    "move message to the trash"

macro index S \
    "<save-message>+Personal/Spam<enter>"  \
        "mark message as spam"
```

**~/.mutt/accounts/work**

```
set from      = "work-username@gmail.com"
set sendmail  = "/usr/bin/msmtp -a work"
set mbox      = "+Work/archive"
set postponed = "+Work/drafts"

color status cyan default

macro index D \
    "<save-message>+Work/Trash<enter>" \
    "move message to the trash"

macro index S \
    "<save-message>+Work/Spam<enter>"  \
        "mark message as spam"
```

<div class="well">
Notice the `color` line which changes the status bar depending on what 
account I'm "in" at any given moment.
</div>

The following settings will tell mutt to source one of these files upon 
entering a folder matching the given pattern, this will setup all the 
correct settings when entering a folder for a given account:

**~/.muttrc**

```
set spoolfile = "+Personal/INBOX"

source ~/.mutt/personal

folder-hook Personal/* source ~/.mutt/accounts/personal
folder-hook Work/*     source ~/.mutt/accounts/work
```

The first two lines effectively set Personal as the default account when 
we open mutt.

Well, that should do it. Open up mutt, change folders, send some mails, 
and make sure everything's working as you'd expect.

For reference, my complete and current setup can be found with my 
[dotfiles][].

[dotfiles]: https://github.com/pbrisbin/dotfiles/tree/master/tag-mail-recipient
