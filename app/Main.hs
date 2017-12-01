{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Hakyll
import Site.IndexedRoute
import Skylighting (pygments, styleToCss)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.XML (Node(..))

main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" $ fromCapture "tags/*/index.html"

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss pygments

    match ("favicon.ico" .||. "css/**") $ do
        route idRoute
        compile copyFileCompiler

    match "posts/*" $ do
        route $ setExtension "" `composeRoutes` indexedRoute

        compile $ do
            let ctx = postCtx tags

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
                    , constField "title" $ "archives on " <> siteTitle
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
                    , constField "title" siteTitle
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
                    , constField "title" siteTitle
                    , constField "root" siteHost
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/feed.xml" ctx
                >>= replaceIndexURLs siteHost

    match "templates/*" $ compile templateCompiler

siteHost :: String
siteHost = "https://pbrisbin.com"

siteTitle :: String
siteTitle = "pbrisbin dot com"

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%d %b %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

feedItemCtx :: Context String
feedItemCtx = mconcat
    [ dateField "date" "%a, %d %b %Y %H:%M:%S %z"
    , constField "root" siteHost
    , mapContext escapeXml $ bodyField "body"
    , defaultContext
    ]

loadContent :: Pattern -> Compiler [Item String]
loadContent p = recentFirst =<< loadAllSnapshots p "content"

escapeXml :: String -> String
escapeXml = renderMarkup . toMarkup . NodeContent . T.pack
