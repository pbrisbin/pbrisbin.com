---
layout: post
title: "For the Library Authors"
tags:
  - yesod
  - haskell
---

Recently, Yesod released version 1.2. You can read the announcement 
[here][announcement], the changelog [here][changelog], and a detailed 
blog post about the subsite rewrite [here][subsite]. These resources do 
a great job of getting users' apps up on 1.2. This post won't rehash 
those, it is in stead intended for those of you (like myself) who 
maintain libraries dependent on Yesod.

[announcement]: http://www.yesodweb.com/blog/2013/05/yesod-1-2-released
[changelog]: https://github.com/yesodweb/yesod/wiki/Changelog#yesod-12-not-yet-released
[subsite]: http://www.yesodweb.com/blog/2013/03/big-subsite-rewrite

The large refactor to the transformer stack and its implications in how 
subsites are written made it non-trivial to port my [markdown][], 
[comments][], and [pagination][] libraries over to 1.2; I imagine many 
other such authors are in the same position and might appreciate a 
little guidance.

I don't claim to know why or how all this stuff works, but at least you 
can benefit from my trial and error.

[markdown]: https://github.com/pbrisbin/yesod-markdown
[comments]: https://github.com/pbrisbin/yesod-comments
[pagination]: https://github.com/pbrisbin/yesod-paginator

<div class="well">
I apologize for the lack of narrative or conclusion here, this is pretty 
much just a list of things I had to take care of during the update 
process...
</div>

### Transformer Stack

You can basically find-and-replace all of these:

```haskell 
fooHandler :: X -> Y -> GHandler s m a
fooWidget  :: X -> Y -> GWidget s m a
```

Into these:

```haskell 
fooHandler :: X -> Y -> HandlerT m IO a
fooWidget  :: X -> Y -> WidgetT m IO a
```

### Lifting

Anywhere you use `lift` to run a `Handler` action from within a `Widget` 
now needs to use `handlerToWidget` for the same purpose.

### Route to Master

Subsites and their masters are now very well isolated, this means you no 
longer need code like this in a master site's hander:

```haskell 
tm <- getRouteToMaster
redirect $ tm SomeRoute
```

It can be simplified to just:

```haskell 
redirect SomeRoute
```

Which is way better.

The function `getRouteToMaster` does still exist as `getRouteToParent`, 
and it should be used (only) to route to a master site's route from 
within the subsite's handler.

### Subsite Declaration

If you author a subsite, here is where your largest changes will be. 
There's a handy [demo app][demo] which serves as a great reference.

[demo]: https://github.com/yesodweb/yesod/tree/new-subsite/demo

Subsites now have a two-phase construction much like in `Foundation.hs`. 
So, where you might've had a single module like this:

```haskell 
module CommentsAdmin
  ( CommentsAdmin
  , getCommentsAdmin
  , Route(..)
  ) where

CommentsAdmin = CommentsAdmin

getCommentsAdmin :: a -> CommentsAdmin
getCommentsAdmin = const CommentsAdmin

mkYesodSub "CommentsAdmin"
    [ ClassP ''YesodComments [ VarT $ mkName "master" ] ]
    [parseRoutes|
        /                            CommentsR      GET
        /edit/#ThreadId/#CommentId   EditCommentR   GET POST
        /delete/#ThreadId/#CommentId DeleteCommentR GET POST
        |]
```

You now need a separate file to define the routes:

**CommentsAdmin/Routes.hs**

```haskell 
module CommentsAdmin.Routes where

CommentsAdmin = CommentsAdmin

mkYesodSubData "CommentsAdmin" [parseRoutes|
    /                            CommentsR      GET
    /edit/#ThreadId/#CommentId   EditCommentR   GET POST
    /delete/#ThreadId/#CommentId DeleteCommentR GET POST
    |]
```

And import/use them separately:

**CommentsAdmin.hs**

```haskell 
module Foo
  ( CommentsAdmin
  , getCommentsAdmin
  , module CommentsAdmin.Routes
  ) where

import CommentsAdmin.Routes

getCommentsAdmin :: a -> CommentsAdmin
getCommentsAdmin = const CommentsAdmin

instance YesodComments m => YesodSubDispatch CommentsAdmin (HandlerT m IO)
    where yesodSubDispatch = $(mkYesodSubDispatch resourcesCommentsAdmin)
```

<div class="well">
There's probably a way around this, but I had enough wrestling to do.
</div>

You'll also want to make a `Handler` synonym for your subsite routes:

```haskell 
type Handler a = forall master. YesodComments master
               => HandlerT CommentsAdmin (HandlerT master IO) a

getCommentsR :: Handler RepHtml
```

It's fine to use `Handler` as long as you don't export it.

### Subsite Actions

What you do from within a subsite will definitely need some tweaking, 
but that's mostly because the old way was very klunky and the new way is 
much cleaner.

If you want to call any functions in the context of the main site, just 
use `lift`. Usually, this'll be `lift $ defaultLayout`, but also, as may 
be common, if you have a Typeclass on your master site providing some 
functionality (like loading comments), you need to use `lift` to call 
those functions from within subsite handlers.

### Persistent Fields

If you `derive PersistField` also now `derive PersistFieldSql`. I don't 
know the motivation behind the split, but as a user dog-fooding my own 
library, I soon realized I needed both instances on my `Markdown` type.

### Persistent Actions

If you have a library exposing functions which are meant to be called 
within `runDB`, you probably already know those type signatures can get 
messy.

Well, they stay messy, but at least I can tell you what you need to 
change. Mine went from this:

```haskell 
selectPaginated :: ( PersistEntity val
                   , PersistQuery m1
                   , PersistEntityBackend val ~ PersistMonadBackend m1
                   , MonadLift (GHandler s m) m1
                => Int
                -> [Filter val]
                -> [SelectOpt val]
                -> m1 ([Entity val], GWidget s m ())
```

To this:

```haskell 
selectPaginated :: ( PersistEntity val
                   , (PersistQuery (YesodPersistBackend m (HandlerT m IO)))
                   , (PersistMonadBackend (YesodPersistBackend m (HandlerT m IO)) ~ PersistEntityBackend val)
                   , (MonadTrans (YesodPersistBackend m))
                   , Yesod m
                   )
                => Int
                -> [Filter val]
                -> [SelectOpt val]
                -> YesodDB m ([Entity val], WidgetT m IO ())
```

I probably could've added the `Yesod m` constraint and used the 
`YesodDB` alias prior to 1.2, but oh well.
