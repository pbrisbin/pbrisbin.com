---
layout: post
title: "Cloud Music: A Comparison"
tags:
  - cloud
  - rdio
  - google
  - music
---

For the longest time, my interaction with music was via MPD. I had a 
substantial collection playing on my always-on desktop. I could connect 
from anywhere to control the playlist and pick up the stream via its 
HTTP output. I had effectively built my own cloud music service.

While that was great, there were a few annoyances:

First, I had to juggle two interfaces while listening remotely. One to 
actually manipulate the playlist and another to hear the stream. Second, 
and more importantly, my collection was stagnant. I wanted smart 
recommendations, and I wanted a simple way to click through and add what 
I liked to my collection.

So I decided to give [Rdio][] a try, and eventually [Google Play][play], 
before coming back to Rdio Overall, the experiences with each were 
about the same. I'm very happy to be using *any* cloud music service and 
would recommend that unquestionably. As for which of the two is better, 
I'm not sure I can offer an objective opinion on the matter. What I can 
do is provide some details about what I liked (or didn't) about each.

[rdio]: http://rdio.com
[play]: https://play.google.com/music

## It doesn't much matter

Seriously, the benefits I experienced from moving to *any* cloud music 
service were far greater than any differences I saw between Rdio and 
Play.

They all manage a collection of music in the cloud which can be easily 
shared and listened to. They all do pandora-esque "stations" built from 
a seed artist, album, or song. They all do recommendations. They all 
have apps. They are all about the same cost.

## Why Rdio

The biggest reason I prefer Rdio is that it maintains a persistent, 
server-side playlist. I like that I can hit pause on my desktop before 
heading to work, pickup the same song in the same spot on my phone as I 
step out the door, then do the same thing when I get to work and log in 
from there.

With Play, each session seemed to have its own playlist. I would also 
have to hard-refresh the browser occasionally to clear some transient 
connection bug leading again to a loss of play state. Contrast that with 
Rdio where, after trying Play for 2 months, I came back to the same 
playlist, paused on the same song, and even with the same volume 
setting. Color me impressed.

Surprisingly, Rdio also gets the edge in social features. I'm not hugely 
into "social", but it is nice to follow people and have a "buddy list" 
visible where you can see who's on and what they're listening to. To be 
fair, this became a larger factor when seemingly half of my company 
joined Rdio one day.

The Rdio web-app generally works better for me than Play. The fact that 
I can close, open, and refresh at will while only noticing a small pause 
in the audio stream is super impressive. Play on the other hand 
frequently froze or had "trouble connecting" and did a generally worse 
job of maintaining playlist and stream state across network events.

For developers, Rdio is certainly better. It has a mature API for 
interacting with all of your data and has a number of wrapper libraries 
in most major programming languages. I've done little in this area so 
far, but it has been useful.

## Why Play

Just like Rdio has that "one great thing" that is its persistent 
playlist, Play too has one great thing: support for uploading your own 
collection.

Rdio will match your iTunes library, but I don't use iTunes (though I 
did eventually [scratch][rdin] that itch). Play on the other hand will 
match *any* music library (by importing from some exotic thing known as 
**a directory of music files**). It even has support for linux. More 
importantly, if there are songs in your collection which it does not 
have in its library, Play will add them to your collection anyway by 
copying your file up to the cloud.

[rdin]: https://github.com/pbrisbin/rdin

As one might expect, Play also did better for me on recommendations. 
This is entirely subjective, so your mileage may vary. Relatedly, I 
really liked Play's "Listen now" view. It's a mishmash of recently added 
or recommended albums and stations. I could consistently go to this 
view, click on the most prominent tile, and be happy with the result. 
Again, subjective, but it is what it is.

Finally, the Play Android app worked better for me (and I should hope 
so, we are talking about Google). On the other hand, Rdio seems to 
iterate rapidly on their Android app and I haven't used it much since 
coming back from Play, so they may have smoothed things out by now.

## You decide

In the end, there's not much risk in trying them out and deciding which 
you like best. Turning the subscriptions on and off at will is easy 
enough and you don't lose any data when doing so (making it easy to come 
back after trying something else).
