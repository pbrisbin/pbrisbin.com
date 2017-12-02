---
title: Site Migration
tags: haskell, self
---

    20:24 rson: if there is anything i could ever suggest that you'd listen to, let
                it be this.  do it.

Wise words from someone who's been there before. That's [rson][] telling 
me that I should move my site to some sort of framework. Make things 
cleaner, easier to maintain, and get away from that goddamn [php][] I 
seem to be so fond of.

I had been thinking about doing this myself for quite some time. As 
silly as it sounds, I was unhappy with my urls. The whole site (from a 
purely url-appearance standpoint) was inconsistent. I dreamed for 
`/feed/` and `/posts/my_post/`.

I could also feel my spider web of php and html spiralling away from me. 
I was spending too much time monitoring comments, tweaking the syntax 
highlighting, and figuring out the best way to format bread crumbs based 
on not only filepath but also custom translations from `content.php` to 
`all posts` and similar.

## Yesod

Then I found [Yesod][yesod-docs], a web framework in Haskell. As anyone who's
ever been to this site knows, I love Haskell. It's just a cool language. So if I
were going to move to some sort of framework, this would be it.

So, using the [Yesod Docs][yesod-docs], the [haddock 
documentation][haddocks], and even the [actual source][docs-source] for 
the Yesod Docs, I was able to hobble my site over to the framework. It 
wasn't easy, but there's a lot of benefit there.

My breadcrumbs went from 100 lines of php to about 14 lines of Haskell. 
And those 14 lines are simply defining what Routes are children of what 
other Routes.

My posts have tags now. This extra bit of post-metadata was even added 
later without disrupting any existing code.

My Rss feed is dynamically created whenever it's loaded.

And probably most important of all, urls used throughout the site are 
*type safe*, compile-time-guaranteed to be valid.

What that means is that I don't type the url directly, I insert a 
Haskell function that corresponds to those pages' Routes. And no, they 
aren't built from regular expressions; each Route is generated as a 
distinct type as defined by me.

Routes can also have arguments. Right now you're viewing the output of 
the `PostR` Route using `site_migration` as its argument. But the best 
part of all that is that the compiler validates every link in my site 
each time it's compiled to ensure it's in scope and type checks!

## Sell Out!

As part of the transition, I'm also giving up some control over code 
snippets and comments. I enjoyed the DIY approach but it was getting 
cumbersome (and less and less KISS as things went on).

Instead, I'm stealing two more ideas from the Yesod Docs site. The new 
site uses git's [gist][] feature for code snippets and [disqus][] for 
comments. I know, I originally said I, "didn't want to farm comments out 
to 3rd party javascript," but disqus is really nice and I'm getting sick 
of all the overhead that comes with my homebrew php setup.

I'm really sorry to anyone who's left comments so far on the site.  I 
appreciate them greatly. I still have them and I'll continue to look 
into ways to port them over to disqus, but so far, it's not looking too 
promising.

<div class="well">
I've changed my approach to posts and am now using pandoc to write them. 
This means that I don't need gist anymore thanks to pandoc's great 
syntax highlighting features. I'm also working on my own Yesod module 
for [Comments][yesod-comments] to get things back the way it was on the 
old site. That's a bit of a work in progress at the moment and will be 
its own post when it's done... I'll be keeping disqus around for a 
while.
</div>

## Lighttpd

Another change I'm making is from Apache over to [Lighttpd][lighttpd] 
(pronounced: *lighty*). To be honest, I just couldn't get (Fast)CGI 
working with apache and I had it running with lighttpd in minutes. 
Hopefully it'll be faster and easier to maintain too, we'll see...

So anyway, enjoy the new site; let me know if anything is broken or 
missing -- I'm still in the process of migrating old posts, so give me 
some time before reporting that.

The site's source is also in my [git repo][site-source] if anyone's 
interested.

[rson]: http://rsontech.net                    "rson tech"
[php]:  http://arch.har-ikkje.net/gfx/php.jpeg "de-motivational php"

[yesod-docs]:  http://www.yesodweb.com/                 "yesod docs"
[haddocks]:    http://hackage.haskell.org/package/yesod "yesod haddocks"
[docs-source]: http://github.com/snoyberg/yesoddocs     "yesoddocs source code"

[recent posts]: /#Recent_Posts "recent posts"
[all posts]:    /posts/        "all posts"
[all tags]:     /tags/         "all tags"

[gist]:   http://gist.github.com/ "gist on github"
[disqus]: http://disqus.com       "disqus"

[yesod-comments]: http://github.com/pbrisbin/yesod-comments "yesod comments"
[lighttpd]:       http://www.lighttpd.net/                  "lighttpd"
[site-source]:    http://github.com/pbrisbin/devsite        "devsite on github"
