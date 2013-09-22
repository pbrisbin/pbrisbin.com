---
layout: post
title: "On Staticness"
tags:
  - meta
  - system
  - jekyll
  - pandoc
---

For almost 7 years, now I've had a desktop at home, running, serving 
(among many things) my personal blog. Doing so is how I learned much of 
what I now know about programming and system administration. It gave me 
a reason to learn HTML, then PHP, then finally Haskell. It taught me 
Postgres, Apache, then lighttpd, then nginx. Without maintaining this 
site myself, on my own desktop, I doubt I would've been sucked into 
these things and I may not have ended up where I am today.

However, I'm now a happily employed Developer and I do these things all 
day on other people's machines and sites. Don't get me wrong, I enjoy it 
all very much, but the educational value of maintaining my personal blog 
as a locally-hosted web-app is just not there any more. With that value 
gone, things like power outages, harddrive failures, Comcast, etc which 
bring my site down become unacceptable. It's too easy to have something 
which requires almost no maintenance while still giving me the control 
and work-flow I want.

I realize I could've moved my site as-is to a VPS and no longer been at 
the whim of Comcast and NSTAR, but that wouldn't decrease the 
maintenance burden enough. Contrast pretty much any typical web-app 
ecosystem with...

The services now required to host my blog:

```
nginx
```

The configuration required to host my blog:

```
$ wc -l < /etc/nginx/nginx.conf
19
```

Adding a new post:

```
$ cat > _posts/2013-09-21-awesome_post.md <<EOF
---
title: "Awesome Post"
tags:
  - some
  - tags
---

Pretty *awesome*.

EOF
```

Deployment:

```
$ jekyll build && rsync -a -e ssh _site/ pbrisbin.com:/srv/http/site/
```

Backups:

```
$ tar czf ~/site.backup _site
```

## Comments

The only two features I lost during the transition were RSS and 
Comments. I'll definitely bring RSS back soon, but the Comments can 
never be (at least not without Disqus -- but, meh). To all those that 
have commented on this site in the past, I apologize. That feature was 
just not worth maintaining a dynamic blog-as-web-app.

I realized that the vast majority of comments on the site were one of 
three things:

1. Hey, nice post!
2. Hey, here's a correction
3. Hey, here's something additional about this topic

These are all useful things, but there's never any real discussion going 
on between commenters; it's all just a note to me. So I've decided to 
let these come in as emails. My hope is folks who might've commented are 
OK sending it in an email. The address is in the footer pretty much 
where you'd expected a Comments section to be. I'll make sure that any 
corrections or additional info sent via email will make it back into the 
main content of the post.

## Pandoc

At some point during this process, I realized that I simply can't 
convert my post markdown to html without [pandoc][]. Every single 
markdown implementation I've found gets the following wrong:

[pandoc]: http://johnmacfarlane.net/pandoc/

```
<div class="something">
I want this content to **also** be parsed as markdown.
</div>
```

Pandoc does it right. Everything else puts the literal text inside the 
`div`. This breaks all my posts horribly because I'll frequently do 
something like:

<div class="note">
This is in a `div` with `class="note"`, and the content inside is 
**still** markdown.
</div>

I had assumed that to get pandoc support I'd have to use [Hakyll][], but 
(at least from the docs) it seemed to be missing tags and next/previous 
link support. It appears extensible enough that I might code that in 
custom, but again, I'm trying to decrease overall effort here. 
[Jekyll][], on the other hand, had those features already *and* let me 
use pandoc easily by dropping [a small ruby file][plugin] in \_plugins.

[hakyll]: http://jaspervdj.be/hakyll/
[jekyll]: http://jekyllrb.com/
[plugin]: https://github.com/pbrisbin/pbrisbin.com/blob/master/_plugins/pandoc_converter.rb

With the conversion complete, I was able to shut down a bunch of 
services on my desktop and even cancel a dynamic DNS account. At 
$5/month, the [Digital Ocean][digitalocean] VPS is a steal. The site's 
faster, more reliable, easier to deploy, and even got a small facelift.

[digitalocean]: https://www.digitalocean.com/

Hopefully the loss of Comments doesn't upset any readers. I love email, 
so please send those comments to **me at pbrisbin dot com**.
