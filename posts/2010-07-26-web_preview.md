---
title: Web Preview
tags: linux
---

Recently, I made the switch (again) away from Uzbl as my main browser. 
Jumanji is a really nice browser in that it's as light as Uzbl but feels 
more polished. It provides almost all of the features I had to build 
into Uzbl myself right out of the box. The tab-completion on the 
commands and urls is incredibly useful and negates the need for all the 
external history and bookmark scripts that I was using with Uzbl. The 
only part I really miss is (obviously) the controllability and 
configurability.

I only ever used this controllability for one thing: previewing web 
pages as I write them. I had a nice little script that would go out and 
ask each Uzbl instance what its URI was, and if it matched the URI 
version of the filename I was currently editing it would send the reload 
command to this browser.

You just cannot do something like this in any other browser.

So I figured, if I relegate Uzbl to this one single simple use, it's 
configurability could be leveraged such that I could strip out anything 
that didn't serve this one purpose and the browser would be incredibly 
responsive.

In the end, I'm actually amazed at how well this worked out. During my 
testing, I actually spent a good ten minutes troubleshooting a 
nonexistent bug because the page was reloading so fast that I thought 
nothing was happening.

This works nicely for me because my desktop is my web server. All I have 
to do is `vim /srv/http/pages/foo.html` and I'm editing 
`http://localhost/pages/foo.html` directly.

<div class="well">
I'm not saying it's impossible to pull this off with a remote server, 
this just makes things easier. It's up to you to port my script for use 
in a remote server setting.
</div>

First thing you'll need is my 
[script](http://github.com/pbrisbin/scripts/blob/master/webpreview), 
download the raw version into your `$PATH`.

Adjust the in-script variables `srv_dir` and `srv_url` to match your 
environment. These variables are used to turn a filename like 
`/srv/http/pages/foo.html` into a url like 
`http://localhost/pages/foo.html.`

<div class="well">
Recently the script has changed slightly to work with my new framework; 
I now just define `file_url` as a direct modification of `$2`.
</div>

Make sure you've got uzbl installed and `uzbl-core` is also in your 
`$PATH`.

Add the following uber simplistic configuration file for uzbl at 
`~/.config/uzbl/config`:

    set socket_dir         = /tmp
    set status_background  = #303030
    set uri_section        = \@[\@uri]\@
    set status_format      = <span font_family="Verdana" foreground="#909090">@uri_section</span>
    set title_format_short = Uzbl - \@TITLE
    set title_format_long  = @title_format_short

This just makes sure a socket is placed in `/tmp` and makes the status 
bar a little more pleasing on the eyes.

<div class="well">
Only the `socket_dir` declaration is actually needed for the script to 
function.
</div>

Finally, add the following to your `~/.vimrc`:

```vim
command! Open :! webpreview --open %
command! Reload :! webpreview --reload %

au BufWritePost /srv/http/pages/* silent Reload
```

This defines an `Open` and `Reload` command to be used directly within 
vim and also sets up an auto command to fire whenever I hit `:w` on a 
page I'm editing.

In your `~/.vimrc` you could make these conditional for html and php 
filetypes and, as you can see, the auto-refresh only happens if I'm 
editing a file under my server's pages directory. You'll want to do 
something similar so that the script doesn't run for all files all the 
time.

That's all that's needed. Fire up your favorite text editor and give it 
a try.
