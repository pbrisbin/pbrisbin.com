---
layout: post
title: "Parsing DATABASE_URL"
tags:
  - yesod
  - haskell
  - heroku
  - postgresql
---

A while back, I made a [post][] about deploying yesod apps to heroku. 
The method used back then is no longer required (thank God!) and 
deploying to heroku is super simple these days. So simple, in fact, that 
I won't reiterate those instructions here, this post is about something 
a bit more specific.

[post]: /posts/deploying_yesod_apps_on_heroku

Chances are, your app is using a database. And you probably don't want 
to hard-code those database credentials in your (probably shared) source 
code. What you'd rather do is parse them out of the `DATABASE_URL` 
environment variable provided by heroku.

Well, here is how you do that:

## herokuConf

Eventually, I might wrap this up in a cabal package you can install, but 
for now just create a helper like this:

**Helpers/Heroku.hs**

```haskell 
module Helpers.Heroku (herokuConf) where

import Prelude
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (PostgresConf(..))
import Web.Heroku (dbConnParams)

import qualified Data.Text as T

herokuConf :: IO PostgresConf
herokuConf = do
    params <- dbConnParams

    return PostgresConf
        { pgConnStr  = formatParams params
        , pgPoolSize = 10 -- Adjust this as you see fit!
        }

    where
        formatParams :: [(Text, Text)] -> ByteString
        formatParams = encodeUtf8 . T.unwords . map toKeyValue

toKeyValue :: (Text, Text) -> Text
toKeyValue (k, v) = k `T.append` "=" `T.append` v
```

<div class="note">
This relies on the [heroku][] package, so be sure you add that to the 
build-depends in your cabal file.
</div>

[heroku]: http://hackage.haskell.org/package/heroku-0.1

## makeFoundation

Now, modify your application loading like so:

**Application.hs**

```haskell 
import Helpers.Heroku

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    -- ...

    dbconf <- if development
                -- default behavior when in development
                then withYamlEnvironment "config/postgresql.yml" (appEnv conf)
                    Database.Persist.loadConfig >>=
                    Database.Persist.applyEnv

                -- but parse DATABASE_URL in non-development
                else herokuConf

    -- ...

    return foundation
```

That's it. Commit, push, enjoy!
