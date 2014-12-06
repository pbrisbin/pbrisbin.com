{-# LANGUAGE OverloadedStrings #-}
module Main where

import Hakyll

import Control.Applicative ((<$>))
import Data.Binary (Binary)
import Data.List (intersperse)
import Data.Monoid (mconcat)
import Data.Typeable (Typeable)
import System.FilePath (dropExtension, splitFileName, takeDirectory)
import Text.Blaze (toMarkup)
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Renderer.String (renderMarkup)
import Text.XML (Node(..))

import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

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
        route $ indexRoute $ \fp ->
            let (path, name) = splitFileName fp
            in path ++ drop 11 (dropExtension name)

        compile $ do
            let ctx = mconcat
                    [ mapContext dropFileName $ nextUrlField "next" navigation
                    , mapContext dropFileName $ prevUrlField "prev" navigation
                    , postCtx tags
                    ]

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

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

            makeItem "" >>= loadAndApplyTemplate "templates/feed.xml" ctx

    match "templates/*" $ compile templateCompiler

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ dateField "date" "%d %b %Y"
    , tagsFieldWith getTags renderTag (mconcat . intersperse ", ") "tags" tags
    , indexedUrlField "url"
    , defaultContext
    ]

feedItemCtx :: Context String
feedItemCtx = mconcat
    [ dateField "date" "%a, %d %b %Y %H:%M:%S %z"
    , constField "root" "http://pbrisbin.com"
    , mapContext xmlEscape $ bodyField "body"
    , indexedUrlField "url"
    , defaultContext
    ]

loadContent :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadContent p = recentFirst =<< loadAllSnapshots p "content"

renderTag :: String -> Maybe FilePath -> Maybe H.Html
renderTag tag = fmap $ \fp ->
    H.a ! A.href (toValue $ toUrl $ takeDirectory fp) $ toHtml tag

xmlEscape :: String -> String
xmlEscape = renderMarkup . toMarkup . NodeContent . T.pack
