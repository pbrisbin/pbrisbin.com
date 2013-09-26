---
layout: post
title: "Email Encryption"
tags:
  - mutt
  - encryption
  - gpg
---

The recent hullabaloo with Snowden and the NSA is very scary. I agree 
with most Americans that The Government is doing some pretty evil things 
these days. That said, I also believe that we as cloud users are 
primarily responsible for the privacy of our own data. Thankfully, the 
problem of transmitting or storing data via a 3rd party without granting 
that party access to said data was [recently][] solved.

[recently]: http://en.wikipedia.org/wiki/Pretty_Good_Privacy

What follows is a high-level walk-through of one such example of 
securing your own privacy when it comes to cloud-based communications: 
encrypted email using [GnuPG][] and [Mutt][].

[gnupg]: http://www.gnupg.org/
[mutt]: http://www.mutt.org/

<div class="well">
This is mainly a regurgitation of [this][guide1] and [this][guide2], so 
I recommend you check out those resources as well.
</div>

[guide1]: http://jasonwryan.com/blog/2013/07/20/gnupg/
[guide2]: http://codesorcery.net/old/mutt/mutt-gnupg-howto

## Signing vs Encrypting

We'll be adding two features to our email repertoire: Signing, which we 
can do all the time, and Encrypting, which we can only do if the person 
with whom we're communicating also supports it.

Signing a message is a way to prove that the message actually came from 
you. The process works by including an attachment which has been 
cryptographically signed using your private key. The recipient can then 
use your public key to verify that signature. Successful verification 
doesn't prove the message came from you per se, but it does prove that 
it came from someone who has access to your private key.

Encryption, on the other hand, is a way to send a message which only the 
intended recipient can read. To accomplish this, the sender encrypts the 
message using the recipient's public key. This means that only someone 
in possession of the corresponding private key (i.e. the recipient 
themselves) can decrypt and read the message.

## How Do I Encryption?

The first step is generating your Key Pair:

```
$ gpg --gen-key
```

The prompts are fairly self-explanatory. I suggest choosing a one year 
expiration and be sure to give it a strong pass-phrase. After this has 
finished, take note of your Key ID which is the value after the slash in 
the following output:

```
$ gpg --list-keys
/home/patrick/.gnupg/pubring.gpg
--------------------------------
pub   2048R/CEC8925D 2013-08-16 [expires: 2014-08-16]
uid                  Patrick Brisbin <pbrisbin@gmail.com>
sub   2048R/33868FEC 2013-08-16 [expires: 2014-08-16]
```

For example, my Key ID is `CEC8925D`.

The next step is to put your public key on a key server so anyone can 
find it when they wish to verify your signatures or send you encrypted 
messages:

```
$ gpg --keyserver hkp://subkeys.pgp.net --send-keys <Key ID>
```

At this point we have all we would need to manually use the `gpg` 
command to encrypt or decrypt documents, but that makes for a clumsy 
emailing process. In stead, we're going to tell Mutt how to execute 
these commands for us as they're needed.

Mutt ships with a sample configuration file which specifies the various 
crypto-related commands for using GnuPG. Since I have no need to tweak 
these settings, I just source this sample file as-is, then go on to set 
only the options I care about:

```
source /usr/share/doc/mutt/samples/gpg.rc

set pgp_timeout = 3600       # how long to cache the pass-phrase

set crypt_autosign = yes     # automatically sign all outgoing mail

set crypt_replyencrypt = yes # automatically encrypt replies to 
                             # encrypted messages

set pgp_sign_as = CEC8925D   # my Key ID
```

That's it -- you're all set to start having fully encrypted 
conversations.

## Try It Out

To confirm everything is working, restart Mutt and compose a test message 
to yourself. When you get to the compose view (after quitting vim), you 
should see something like the following:

```
Security: Sign (PGP/MIME)
 sign as: CEC8925D
```

This confirms that auto-signing is working and it's using the correct 
key.

Press `p` to enter the (p)gp menu. This menu allows you to remove or 
modify the security-related things you're planning on doing with this 
email. We'll choose `b` to (b)oth sign and encrypt this message.

Upon receiving the test message, the body should look like this:

```
[-- PGP output follows (current time: Tue 20 Aug 2013 04:14:20 PM EDT) --]
gpg: Signature made Fri 16 Aug 2013 11:02:51 AM EDT using RSA key ID CEC8925D
gpg: Good signature from "Patrick Brisbin <pbrisbin@gmail.com>"
[-- End of PGP output --]

[-- The following data is PGP/MIME encrypted --]

Test

--
patrick brisbin

[-- End of PGP/MIME encrypted data --]
```

You can see here the message signature was verified and the body came in 
as encrypted and was successfully decrypted and presented to us by Mutt. 
This means just about everything's working. To test the final piece, go 
ahead and reply to this message. Back in the compose view, you should 
see this:

```
Security: Sign, Encrypt (PGP/MIME)
 sign as: CEC8925D
```

This confirms the last piece of the puzzle: replies to encrypted 
messages are automatically encrypted as well.

Hopefully, this post has shown just how easy it is to have secure, 
private communication. And you don't even have to ditch Gmail! All you 
need is a decent client and a little bit of setup. Now send me some 
encrypted secrets!
