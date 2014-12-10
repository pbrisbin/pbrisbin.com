{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.Monoid (mconcat)
import Data.Typeable (Typeable)
import System.FilePath (dropExtension, splitFileName, takeDirectory)
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.XML (Node(..))

import qualified Data.Text as T

import Navigation

main :: IO ()
main = hakyll $ do
    tags <- buildTags "posts/*" $ fromCapture "tags/*/index.html"
    navigation <- buildNavigation "posts/*"

    -- Static files
    match ("favicon.ico" .||. "css/*" .||. "img/*") $ do
        route idRoute
        compile copyFileCompiler

    -- Post pages
    match "posts/*" $ do
        route $ customRoute $ \i ->
            let (path, name) = splitFileName $ toFilePath i
            in path ++ drop 11 (dropExtension name) ++ "/index.html"

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
                >>= stripIndexUrls

    -- Archives
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
                >>= stripIndexUrls

    -- Tags
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
                >>= stripIndexUrls

    -- Homepage
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
                >>= stripIndexUrls

    -- RSS
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
                >>= stripIndexUrls

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

loadContent :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadContent p = recentFirst =<< loadAllSnapshots p "content"

stripIndexUrls :: Item String -> Compiler (Item String)
stripIndexUrls = return . fmap go

  where
    go = replaceAll "(http://pbrisbin.com|\")/[^\"]*/index.html" takeDirectory

escapeXml :: String -> String
escapeXml = renderMarkup . toMarkup . NodeContent . T.pack
