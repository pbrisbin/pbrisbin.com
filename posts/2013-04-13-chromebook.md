---
title: Chromebook
tags: cloud, development
---

I've heard rumors that a Google [Chromebook][] can make a surprisingly 
sweet machine for a developer. As someone that works exclusively in the 
console, it's easy enough to SSH into a server to do the actual work. 
Since my apps are either command line tools or web sites, I can easily 
test them remotely as well. I only need something with a terminal and 
browser... And reliable internet.

[Chromebook]: http://www.samsung.com/us/computer/chromebook

This workflow is very attractive to me: you get a conveniently portable 
device with great battery life on which to work, it's cheap and 
essentially disposable should anything happen to it, and the machine 
where you actually develop can be specialized to the task. I already 
have such a machine available, but you could also get a fairly 
affordable linode or even spin up an EC2 instance.

Right out of the box, things work quite well. The [Secure 
Shell][secure-shell] browser extension can give you an xterm-compliant 
terminal directly in a browser tab. You could effectively be writing web 
pages in a web page. Crazy.

[secure-shell]: https://chrome.google.com/webstore/detail/secure-shell/pnhechapfaindjhompbnflcldabbghjo

I decided to go a bit further and enable what's called "Developer mode" 
this gives you a real bash shell and a real `ssh` command. I found the 
actual terminal to be a bit stabler, especially port forwarding.

## Enabling Developer Mode

On older versions this is a physical toggle, but in the version I have 
it's a software switch:

<div class="well">
This process is effectively a factory reset. Though, if you have data on 
there you're worried about, you're Doing It Wrong.
</div>

1. Hold Escape and Refresh, then press Power
2. At the blank screen, press Ctrl-D
3. Follow the prompts and enter into "Unverified" mode

The machine will eventually reboot and run some process before 
presenting you again with the initial setup screen.

You're going to see a warning each time the machine boots about being in 
this mode. I was hoping to only see it the first boot after enabling it, 
but that doesn't seem to be the case. The warning times out after 30 
seconds or you can press Ctrl-D to dismiss it immediately. Note that you 
need to use the real Ctrl key for this. For example, I've mapped the 
Search key to Ctrl, but it's not recognized as such for this 
functionality.

## Getting Shell

The quasi-terminal is accessed by pressing Alt-Ctrl-t. From here, type 
`shell` to start a bash shell. This is an environment any linux user 
should be used to, with only a few surprises.

Be sure to install the [Crosh Window][crosh-window] browser extension. 
It allows you to pull the browser-tab terminal out into its own window. 
Without it, many important key bindings will be swallowed by the 
browser.

[crosh-window]: https://chrome.google.com/webstore/detail/crosh-window/nhbmpbdladcchdhkemlojfjdknjadhmh

The shell opens in `/` but you do have a proper `~/` at 
`/home/chronos/user`. Let's `cd` there and setup a few niceties.

<div class="well">
The bundled `vim` is built with only the "tiny" featureset and is not 
very fun to use. I might even go so far as to recommend `cat`ting the 
content into these files.
</div>

First, setup an `~/.ssh/config`:

```
Host example.com # your dev box in the cloud
  User username  # so you don't need user@example.com
  SendEnv TERM   # we'll get to this in a second
```

The actual `TERM` is reported as `linux` and that leads to a pretty bad 
experience on the remote machine. We'll set a custom one and send it via 
`SendEnv`. We don't want to just `export` it, or the actual chrome shell 
acts funny, we'll just set it on the commandline when invoking `ssh`.

That leads me to `~/.bashrc` where we define a `connect` function for 
getting into our dev box:

```bash 
# add to the bottom of the file:
connect() {
  TERM=xterm-256color ssh -L 3000:localhost:3000 example.com
}
```

We set that custom `TERM` variable and use the `-L` option to forward 
port 3000. This means that when I start up the web application I'm 
developing on "in the cloud", I can access it from the chromebook's 
browser at `http://localhost:3000/`.

## SSH Keys

I don't allow just password access to my machine(s), and prefer to use 
RSA keys pairs. Setting this up for the Chromebook was also super easy:

1. Generate a key pair, etc
2. Upload the private key to Google Drive
3. Move it from Drive to Downloads on the Chromebook
4. Move it from `~/Downloads` to `~/.ssh/id_rsa` in the shell

I typically name my keys meaningfully since I use quite a few of them 
for various hosts. If you do the same, just add an `IdentityFile` clause 
to your ssh config.

## Conclusion

I'm very happy with this setup. The Chromebook is literally filling the 
exact same role previously held by a Macbook Air, but for a fraction of 
the price and with better battery life. This terminal is certainly 
better than Terminal.app and might even be better than iTerm2.


![chromebook shell](http://images.pbrisbin.com/chromebook/chromebook_shell.png)\ 

![chromebook htop](http://images.pbrisbin.com/chromebook/chromebook_htop.png)\ 

In short, do eeeeet.
