---
title: Landlord Reviews
tags: haskell
---

Last weekend, when our heat wasn't working (again), I had an idea: *What 
if there were a site where I could bitch about my landlord? Then, people 
who were about to sign a lease could check on this site and see if their 
would-be landlord sucks... before they sign that lease.*

After confirming with <del>the girlfriend</del> marketing that this was 
actually a half-way decent idea, I started to get excited about it. A 
few short hours later and I had a decent mock-up.

**Full disclosure**: It's at that point that we found 
[ratemylandlord.com][rate], *sigh*.

Though pretty deflating, it's not exactly the same. In fact, that site's 
kind of old and doesn't have the super-cool jQuery-ness of mine. I'm 
thinking I could do things differently enough to at least warrant 
putting my site out there.

Personally, I like my <abbr title="user interface">UI</abbr> a lot 
better.

## Live!

I give you... <del>Landlord Reviews</del> [Renters' reality][review]. As 
it is, you can leave a positive or negative review and search reviews by 
landlord name or partial address.

I'm making this post to ask for beta-testers. Go, make some fake 
reviews, play around with the search boxes. If you're feeling motivated, 
report bugs to me via email or on github.

## Todos and Known Bugs

I do have plans to make this epically useful. Any feature requests, just 
send 'em my way.

Here's the current list I've been procrastinating on:

  * Allow "discussion" on review pages
  * More JSON APIs
  * Maps
  * Profit

And some things I already know I need to fix:

  * If you skip a required form field (some browsers allow it), you get 
    a nasty server error.

## The Source

The site is written in haskell (what else) compiled to a fastcgi 
executable. All I have to do is `scp` it up to my slice and it just 
works.

A beautiful thing.

<div class="well">
I use [slicehost][]. They have an Arch image. They rock.
</div>

If you're interested, you can view the source on my [github][]; 
pull-requests always welcome.

[rate]:      http://ratemylandlord.com
[slicehost]: http://www.slicehost.com
[review]:    http://rentersreality.com
[github]:    https://github.com/pbrisbin/bad-boston-landlords
