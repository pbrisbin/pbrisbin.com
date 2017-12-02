---
title: UI Refresh
tags: self
---

Astute readers may have noticed, the site looks a little bit different 
today. I know, it's tough to discern, but if you look closely you might 
see... It's now dark on light!

This is actually a small, tangential change that I made as part of 
a sweeping upgrade and cleanup effort. In moving to Yesod 0.10 (the 1.0 
release candidate), I decided to take an axe to some of the bloatier 
areas of the site.

After dropping a few pounds in backend logic, I decided to keep going 
and attack the css as well -- and by attack, I mean drop entirely.

Believe it or not just about all styling on the site is now coming from 
twitter's awesome bootstrap framework.

Breadcrumbs, notices, login dropdowns, general forms, and sweet tables 
all without a line of styling by me.

The change does make the site less-then-great on less-than-wide 
monitors, but I'm not sure how many people are viewing this on mobile 
devices, etc. We'll see if I need to bring back my `@media` queries.

<div class="well">
Bootstrap 2.0 brings a "responsive" grid, so now the site looks pretty 
good on just about any device.
</div>

I should be posting more in the coming weeks on some of the specific 
changes as well a new search feature I'm hoping to roll out soon, but I 
figured such a noticeable visual change should have an accompanying 
post... So, there it was.
