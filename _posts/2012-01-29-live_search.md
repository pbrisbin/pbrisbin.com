---
layout: post
title: "Live Search"
tags:
  - haskell
  - website
  - yesod
---

I've had some fun recently, adding full-text search support to the posts 
on the site to try and make a simple-but-still-useful archive.

I'd like to post a bit about the feature and how it works. It's got a 
few moving parts so I'm going to break it up a bit.

This post will focus on the backend, setting up [sphinx][search], 
providing content to it from a yesod application, and executing a search 
from within a handler. The second post will go into the front-end 
javascript that I implemented for a pretty simple but effective 
search-as-you-type interface.

[search]: http://sphinxsearch.com/

<div class="note">
For the full context, including required imports and supporting 
packages, please see this feature in the [wild][].

</div>

[wild]: https://github.com/pbrisbin/devsite

## Sphinx

Sphinx is a full-text search tool. This assumes you've got some concept 
of "documents" hanging around with lots of content you want to search 
through by key word.

What sphinx does is let you define a source -- a way to get at all of 
the content you have in a digestible format. It will then consume all 
that content and build an index which you can search very efficiently 
returning a list of Ids. You can then use those Ids to display the 
results to your users.

There are other aspects re: weighting and attributes, but I'm not going 
to go into that here.

The first thing you need to do (after installing sphinx) is to get your 
content into a sphinx-index.

If you've got the complete text you'll be searching actually in your 
database, sphinx can natively pull from mysql or postgresql. In my case, 
the content is stored on disk in markdown files. For such a scenario, 
sphinx allows an "xmlpipe" source.

What this means is that you provide sphinx with a command to fetch an 
xml document containing the content it should index.

Now, if you've got a large amount of content, you're going to want to 
use clever conduit/enumerator tricks to stream the xml to the indexer in 
constant memory. That's what's being done in [this][] example. I'm doing 
something a little bit more naive -- for two reasons:

[this]: http://www.yesodweb.com/wiki/SphinxSearch

1. I need to break out into `IO` to get the content. This is difficult 
   from within a lifted conduit Monad, etc.
2. I don't have that much shit -- the thing indexes in almost no time 
   and using almost no memory even with this naive approach.

So, here's the simple way:

```haskell 
getSearchXmlR :: Handler RepXml
getSearchXmlR = do
    -- select all posts
    posts <- runDB $ selectList [] []

    -- convert each post into an xml block
    blocks <- liftIO $ forM posts $ \post -> do
        docBlock (entityKey post) (entityVal post)

    -- concat those blocks together to one xml document
    fmap RepXml $ htmlToContent $ mconcat blocks

    where
        htmlToContent :: Html -> Handler Content
        htmlToContent = hamletToContent . const

docBlock :: PostId -> Post -> IO Html
docBlock pid post = do
    let file = pandocFile $ postSlug post

    -- content is kept in markdown files on disk, if the file can't be 
    -- found, try to use the in-db description, else just give up.
    exists <- doesFileExist file
    mkd    <- case (exists, postDescr post) of
        (True, _         ) -> markdownFromFile file
        (_   , Just descr) -> return descr
        _                  -> return $ Markdown "nothing?"

    return $
        -- this is the simple document structure expected by sphinx's 
        -- "xmlpipe" source
        [xshamlet|
            <document>
                <id>#{toPathPiece pid}
                <title>#{postTitle post}
                <body>#{markdownToText mkd}
            |]

    where
        markdownToText :: Markdown -> Text
        markdownToText (Markdown s) = T.pack s
```

With this route in place, a sphinx source can be setup like the 
following:

```
source pbrisbin-src
{
	type		= xmlpipe
        xmlpipe_command = curl http://localhost:3001/search/xmlpipe
}

index pbrisbin-idx
{
	source		= pbrisbin-src
	path		= /var/lib/sphinx/data/pbrisbin
	docinfo		= extern
	charset_type	= utf-8
}
```

Notice how I actually hit localhost? Since pbrisbin.com is reverse 
proxied via nginx to 3 warp instances running on 3001 through 3003 
there's no need to go out to the internet, dns, and back through nginx 
-- I can just hit the backend directly.

With that setup, we can do a test search to make sure all is well:

```
$ sphinx-indexer --all # setup the index, ensure no errors
$ sphinx-search mutt
Sphinx 2.1.0-id64-dev (r3051)
Copyright (c) 2001-2011, Andrew Aksyonoff
Copyright (c) 2008-2011, Sphinx Technologies Inc 
(http://sphinxsearch.com)

using config file '/etc/sphinx/sphinx.conf'...
index 'pbrisbin-idx': query 'mutt ': returned 6 matches of 6 total in 
0.000 sec

displaying matches:
1. document=55, weight=2744, gid=1, ts=Wed Dec 31 19:00:01 1969
2. document=62, weight=2728, gid=1, ts=Wed Dec 31 19:00:01 1969
3. document=73, weight=1736, gid=1, ts=Wed Dec 31 19:00:01 1969
4. document=68, weight=1720, gid=1, ts=Wed Dec 31 19:00:01 1969
5. document=56, weight=1691, gid=1, ts=Wed Dec 31 19:00:01 1969
6. document=57, weight=1655, gid=1, ts=Wed Dec 31 19:00:01 1969

words:
1. 'mutt': 6 documents, 103 hits
```

Sweet.

## Haskell

Now we need to be able to execute these searches from haskell. This part 
is actually going to be split up into two sub-parts: first, the 
interface to sphinx which returns a list of `SearchResult`s for a given 
query, and second, the handler to return `JSON` search results to some 
abstract client.

I've started to get used to the following "design pattern" with my yesod 
sites:

*Keep Handlers as small as possible*.

I mean no bigger than this:

```haskell 
getFooR :: Handler RepHtml
getFooR = do
    things      <- getYourThings

    otherThings <- doRouteSpecificStuffTo things

    defaultLayout $ do
        setTitle "..."
        $(widgetFile "...")
```

And that's **it**. Some of my handlers break this rule, but many of them 
fell into it accidentally. I'll be going through and trying to enforce 
it throughout my codebase soon.

For this reason, I've come to love per-handler helpers. Tuck all that 
business logic into a per-handler or per-model (which often means the 
same thing) helper and export a few smartly named functions to call from 
within that skinny handler.

Anyway, I digress -- Here's the sphinx interface implemented as 
`Helpers.Search` leveraging gweber's great [sphinx][] package:

<div class="note">
The below helper actually violates my second "design pattern": *Keep 
Helpers generic* and could be generalized away from anything 
app-specific by simply passing a few extra arguments around. You can see 
a more generic example [here][].

</div>

[sphinx]: http://hackage.haskell.org/package/sphinx
[here]: https://github.com/pbrisbin/renters-reality/blob/master/Helpers/Sphinx.hs

```haskell 
sport :: Int
sport = 9312

index :: String
index = "pbrisbin-idx"

-- here's what I want returned to my Handler
data SearchResult = SearchResult
    { resultSlug    :: Text
    , resultTitle   :: Text
    , resultExcerpt :: Text
    }

-- and here's how I'll get it:
executeSearch :: Text -> Handler [SearchResult]
executeSearch text = do
    res <- liftIO $ query config index (T.unpack text)

    case res of
        Ok sres -> do
            let pids = map (Key . PersistInt64 . documentId) $ matches sres

            posts <- runDB $ selectList [PostId <-. pids] []

            forM posts $ \(Entity _ post) -> do
                excerpt <- liftIO $ do
                    context <- do
                        let file = pandocFile $ postSlug post

                        exists <- doesFileExist file
                        mkd    <- case (exists, postDescr post) of
                            (True, _         ) -> markdownFromFile file
                            (_   , Just descr) -> return descr
                            _                  -> return $ Markdown "nothing?"

                        return $ markdownToString mkd

                    buildExcerpt context (T.unpack text)

                return $ SearchResult
                            { resultSlug    = postSlug post
                            , resultTitle   = postTitle post
                            , resultExcerpt = excerpt
                            }

        _ -> return []

    where
        markdownToString :: Markdown -> String
        markdownToString (Markdown s) = s

        config :: Configuration
        config = defaultConfig
            { port   = sport
            , mode   = Any
            }

-- sphinx can also build excerpts. it doesn't do this as part of the 
-- search itself but once you have your results and some context, you 
-- can ask sphinx to do it after the fact, as I do above.
buildExcerpt :: String -- ^ context
             -> String -- ^ search string
             -> IO Text
buildExcerpt context qstring = do
    excerpt <- buildExcerpts config [concatMap escapeChar context] index qstring
    return $ case excerpt of
        Ok bss -> T.pack $ C8.unpack $ L.concat bss
        _      -> ""

    where
        config :: E.ExcerptConfiguration
        config = E.altConfig { E.port = sport }

        escapeChar :: Char -> String
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar c   = [c]
```

OK, so now that I have a nice clean `executeSearch` which I don't have 
to think about, I can implement a `JSON` route to actually be used by 
clients:

```haskell 
getSearchR :: Text -> Handler RepJson
getSearchR qstring = do
    results <- executeSearch qstring

    objects <- forM results $ \result -> do
        return $ object [ ("slug"   , resultSlug    result)
                        , ("title"  , resultTitle   result)
                        , ("excerpt", resultExcerpt result)
                        ]

    jsonToRepJson $ array objects
```

Gotta love that skinny handler, does its structure look familiar?

You can see the result by visiting [search/j/mutt](/search/j/mutt) for 
example.

In the next post, I'll give you the javascript that consumes this, 
creating the search-as-you-type interface you see on the Archives page.
