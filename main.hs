{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll
import IndexedRoute
import Navigation

import Control.Applicative ((<$>))
import Data.Monoid (mconcat)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.XML (Node(..))

import qualified Data.Text as T

main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" $ fromCapture "tags/*/index.html"
    navigation <- buildNavigation "posts/*"

    match ("favicon.ico" .||. "css/**" .||. "img/**") $ do
        route idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "" `composeRoutes` indexedRoute

        compile $ do
            let ctx = mconcat
                    [ nextUrlField "next" navigation
                    , prevUrlField "prev" navigation
                    , nextTitleField "nextTitle" navigation
                    , prevTitleField "prevTitle" navigation
                    , postCtx tags
                    ]

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    create ["archives/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"

            let ctx = mconcat
                    [ listField "posts" (postCtx tags) (return posts)
                    , constField "title" "archives on pbrisbin dot com"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- loadContent pattern

            let ctx = mconcat
                    [ listField "posts" (postCtx tags) (return posts)
                    , constField "title" tag
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- take 10 <$> loadContent "posts/*"

            let ctx = mconcat
                    [ listField "posts" (postCtx tags) (return posts)
                    , constField "title" "pbrisbin dot com"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    create ["feed/index.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadContent "posts/*"

            let ctx = mconcat
                    [ listField "posts" feedItemCtx (return posts)
                    , constField "title" "pbrisbin dot com"
                    , constField "root" "http://pbrisbin.com"
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/feed.xml" ctx
                >>= replaceIndexURLs "http://pbrisbin.com"

    match "templates/*" $ compile templateCompiler

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%d %b %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedItemCtx :: Context String
feedItemCtx = mconcat
    [ dateField "date" "%a, %d %b %Y %H:%M:%S %z"
    , constField "root" "http://pbrisbin.com"
    , mapContext escapeXml $ bodyField "body"
    , defaultContext
    ]

loadContent :: Pattern -> Compiler [Item String]
loadContent p = recentFirst =<< loadAllSnapshots p "content"

escapeXml :: String -> String
escapeXml = renderMarkup . toMarkup . NodeContent . T.pack
