---
title: Writing JSON APIs with Yesod
tags: haskell, yesod
---

Lately at work, I've been fortunate enough to work on a JSON API which I 
was given the freedom to write in [Yesod][]. I was a bit hesitant at 
first since my only Yesod experience has been richer html-based sites 
and I wasn't sure what support (if any) there was for strictly JSON 
APIs. Rails has a number of conveniences for writing concise controllers 
and standing up APIs quickly -- I was afraid Yesod may be lacking.

[yesod]: http://www.yesodweb.com/

I quickly realized my hesitation was unfounded. The process was 
incredibly smooth and Yesod comes with just as many niceties that allow 
for rapid development and concise code when it comes to JSON-only API 
applications. Couple this with all of the benefits inherent in using 
Haskell, and it becomes clear that Yesod is well-suited to sites of this 
nature.

In this post, I'll outline the process of building such a site, explain 
some conventions I've landed on, and discuss one possible pitfall when 
dealing with model relations.

<div class="well">
**Note**: The code in this tutorial was extracted from a current
[project][carnival] and is in fact working there. However, I haven't
test-compiled the examples exactly as they appear in the post. It's entirely
possible there are typos and the like. Please reach out on Twitter or via email
if you run into any trouble with the examples.
</div>

[carnival]: https://github.com/thoughtbot/carnival

## What We Won't Cover

This post assumes you're familiar with Haskell and Yesod. It also won't 
cover some important but un-interesting aspects of API design. We'll 
give ourselves arbitrary requirements and I'll show only the code 
required to meet those.

Specifically, the following will not be discussed:

* Haskell basics
* Yesod basics
* Authentication
* Embedding relations or side-loading
* Dealing with created-at or updated-at fields

## Getting Started

To begin, let's get a basic Yesod site scaffolded out. How you do this 
is up to you, but here's my preferred steps:

```
$ mkdir ./mysite && cd ./mysite
$ cabal sandbox init
$ cabal install alex happy yesod-bin
$ yesod init --bare
$ cabal install --dependencies-only
$ yesod devel
```

The scaffold comes with a number of features we won't need. You don't 
have to remove them, but if you'd like to, here they are:

* Any existing models
* Any existing routes/templates
* Authentication
* Static file serving

## Models

For our API example, we'll consider a site with posts and comments. 
We'll keep things simple, additional models or attributes would just 
mean more lines in our JSON instances or more handlers of the same basic 
form. This would result in larger examples, but not add any value to the 
tutorial.

Let's go ahead and define the models:

**config/models**

```
Post
  title Text
  content Text

Comment
  post PostId
  content Text
```

## JSON

It's true that we can add a `json` keyword in our model definition and 
get derived `ToJSON`/`FromJSON` instances for free on all of our models; 
we won't do that though. I find these JSON instances, well, ugly. You'll 
probably want your JSON to conform to some conventional format, be it 
[jsonapi][] or Active Model Serializers. Client side frameworks like 
Ember or Angular will have better built-in support if your API conforms 
to something conventional. Writing the instances by hand is also more 
transparent and easily customized later.

[jsonapi]: http://jsonapi.org/

Since what we do doesn't much matter, only that we do it, I'm going to 
write JSON instances and endpoints to appear as they would in a Rails 
project using Active Model Serializers.

**Model.hs**

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- { "id": 1, "title": "A title", "content": "The content" }
instance ToJSON (Entity Post) where
    toJSON (Entity pid p) = object
        [ "id"      .= (String $ toPathPiece pid)
        , "title"   .= postTitle p
        , "content" .= postContent p
        ]

instance FromJSON Post where
    parseJSON (Object o) = Post
        <$> o .: "title"
        <*> o .: "content"

    parseJSON _ = mzero

-- { "id": 1, "post_id": 1, "content": "The comment content" }
instance ToJSON (Entity Comment) where
    toJSON (Entity cid c) = object
        [ "id"      .= (String $ toPathPiece cid)
        , "post_id" .= (String $ toPathPiece $ commentPost c)
        , "content" .= commentContent c
        ]

-- We'll talk about this later
--instance FromJSON Comment where
```

# Routes and Handlers

Let's start with a RESTful endpoint for posts:

**config/routes**

```
/posts         PostsR GET POST
/posts/#PostId PostR  GET PUT DELETE
```

Since our API should return proper status codes, let's add the required 
functions to `Import.hs`, making them available everywhere:

**Import.hs**

```haskell
import Network.HTTP.Types as Import
    ( status200
    , status201
    , status400
    , status403
    , status404
    )
```

Next we write some handlers:

**Handlers/Posts.hs**

```haskell
getPostsR :: Handler Value
getPostsR = do
    posts <- runDB $ selectList [] [] :: Handler [Entity Post]

    return $ object ["posts" .= posts]

postPostsR :: Handler ()
postPostsR = do
    post <- requireJsonBody :: Handler Post
    _    <- runDB $ insert post

    sendResponseStatus status201 ("CREATED" :: Text)
```

You'll notice we need to add a few explicit type annotations. Normally, 
Haskell can infer everything for us, but in this case the reason for the 
annotations is actually pretty interesting. The `selectList` function 
will return any type that's persistable. Normally we would simply treat 
the returned records as a particular type and Haskell would say, "Aha! 
You wanted a Post" and then, as if by time travel, `selectList` would 
give us appropriate results.

In this case, all we do with the returned `posts` is pass them to 
`object`. Since `object` can work with any type than can be represented 
as JSON, Haskell doesn't know which type we mean. We must remove the 
ambiguity with a type annotation somewhere.

**Handlers/Post.hs**

```haskell
getPostR :: PostId -> Handler Value
getPostR pid = do
    post <- runDB $ get404 pid

    return $ object ["post" .= (Entity pid post)]

putPostR :: PostId -> Handler Value
putPostR pid = do
    post <- requireJsonBody :: Handler Post

    runDB $ replace pid post

    sendResponseStatus status200 ("UPDATED" :: Text)

deletePostR :: PostId -> Handler Value
deletePostR pid = do
    runDB $ delete pid

    sendResponseStatus status200 ("DELETED" :: Text)
```

I love how functions like `get404` and `requireJsonBody` allow these 
handlers to be completely free of any error-handling concerns, but still 
be safe and well-behaved.

## Comment Handlers

There's going to be a small annoyance in our comment handlers which I 
alluded to earlier by omitting the `FromJSON` instance on `Comment`. 
Before we get to that, let's take care of the easy stuff:

**config/routes**

```
/posts/#PostId/comments            CommentsR GET POST
/posts/#PostId/comments/#CommentId CommentR  GET PUT DELETE
```

**Handlers/Comments.hs**

```haskell
getCommentsR :: PostId -> Handler Value
getCommentsR pid = do
    comments <- runDB $ selectList [CommentPost ==. pid] []

    return $ object ["comments" .= comments]

-- We'll talk about this later
--postCommentsR :: PostId -> Handler ()
```

For the single-resource handlers, we're going to assume that a 
`CommentId` is unique across posts, so we can ignore the `PostId` in 
these handlers.

**Handlers/Comment.hs**

```haskell
getCommentR :: PostId -> CommentId -> Handler Value
getCommentR _ cid = do
    comment <- runDB $ get404 cid

    return $ object ["comment" .= (Entity cid comment)]

-- We'll talk about this later
--putCommentR :: PostId -> CommentId -> Handler ()

deleteCommentR :: PostId -> CommentId -> Handler ()
deleteCommentR _ cid = do
    runDB $ delete cid

    sendResponseStatus status200 ("DELETED" :: Text)
```

## Handling Relations

Up until now, we've been able to define JSON instances for our model, 
use `requireJsonBody`, and `insert` the result. In this case however, the 
request body will be lacking the Post ID (since it's in the URL). This 
means we need to parse a different but similar data type from the JSON, 
then use that and the URL parameter to build a `Comment`.

**Helpers/Comment.hs**

```haskell
-- This datatype would be richer if Comment had more attributes. For now 
-- we only have to deal with content, so I can use a simple newtype.
newtype CommentAttrs = CommentAttrs Text

instance FromJSON CommentAttrs where
    parseJSON (Object o) = CommentAttrs <$> o .: "content"
    parseJSON _          = mzero

toComment :: PostId -> CommentAttrs -> Comment
toComment pid (CommentAttrs content) = Comment
    { commentPost    = pid
    , commentContent = content
    }
```

This may seem a bit verbose and even redundant, and there's probably a 
more elegant way to get around this situation. Lacking that, I think the 
additional safety (vs the obvious solution of making `commentPost` a 
`Maybe`) and separation of concerns (vs putting this in the model layer) 
is worth the extra typing. It's also very easy to use:

**Handlers/Comments.hs**

```haskell
import Helpers.Comment

postCommentsR :: PostId -> Handler ()
postCommentsR pid = do
    _ <- runDB . insert . toComment pid =<< requireJsonBody

    sendResponseStatus status201 ("CREATED" :: Text)
```

**Handlers/Comment.hs**

```haskell
import Helpers.Comment

putCommentR :: PostId -> CommentId -> Handler ()
putCommentR pid cid = do
    runDB . replace cid . toComment pid =<< requireJsonBody

    sendResponseStatus status200 ("UPDATED" :: Text)
```

<div class="well">
We don't need a type annotation on `requireJsonBody` in this case. Since 
the result is being passed to `toComment pid`, Haskell knows we want a 
`CommentAttrs` and uses its `parseJSON` function within `requireJsonBody`
</div>

## Conclusion

With a relatively small amount of time and code, we've written a 
fully-featured JSON API using Yesod. I think the JSON instances and API 
handlers are more concise and readable than the analogous Rails 
serializers and controllers. Our system is also far safer thanks to the 
type system and framework-provided functions like `get404` and 
`requireJsonBody` without us needing to explicitly deal with any of that.

I hope this post has shown that Yesod is indeed a viable option for 
projects of this nature.
