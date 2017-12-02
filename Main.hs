{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Hakyll
import Skylighting (pygments, styleToCss)
import System.FilePath.Posix (splitFileName)
import qualified System.FilePath.Posix as P
import Text.Blaze (toMarkup)
import Text.Blaze.Renderer.String (renderMarkup)
import Text.XML (Node(..))

main :: IO ()
main = hakyll $ do
    match ("favicon.ico" .||. "css/**") $ do
        route idRoute
        compile copyFileCompiler

    create ["css/syntax.css"] $ do
        route idRoute
        compile $ makeItem $ styleToCss pygments

    -- http://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
    tags <- buildTags "posts/*" $ fromCapture "tags/*/index.html"

    match "posts/*" $ do
        route postRoute
        compile $ do
            let ctx = postCtx tags

            pandocCompiler
                >>= saveSnapshot "content"
                >>= loadAndApplyTemplate "templates/post.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    tagsRules tags $ \tag p -> do
        route idRoute
        compile $ do
            posts <- loadContent p

            let ctx = mconcat
                    [ listField "posts" (postCtx tags) (return posts)
                    , constField "title" tag
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= replaceIndexLinks

    create ["archives/index.html"] $ do
        route idRoute
        compile $ do
            posts <- loadContent "posts/*"

            let ctx = mconcat
                    [ listField "posts" (postCtx tags) (return posts)
                    , constField "title" $ "archives on " <> siteTitle
                    , defaultContext
                    ]

            makeItem ""
                >>= loadAndApplyTemplate "templates/archives.html" ctx
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

-- | Route @foo/bar/{prefix-}baz{.ext}@ to @foo/bar/baz/index.html@
postRoute :: Routes
postRoute = setExtension "" `composeRoutes` indexedRoute
  where
    indexedRoute = customRoute $ \i ->
        let (path, name) = splitFileName $ toFilePath i
        in path P.</> dropDatePrefix name P.</> "index.html"

    dropDatePrefix = drop $ length ("YYYY-MM-DD-" :: String)

feedItemCtx :: Context String
feedItemCtx = mconcat
    [ dateField "date" "%a, %d %b %Y %H:%M:%S %z"
    , constField "root" siteHost
    , mapContext escapeXml $ bodyField "body"
    , defaultContext
    ]
  where
    escapeXml :: String -> String
    escapeXml = renderMarkup . toMarkup . NodeContent . T.pack

loadContent :: Pattern -> Compiler [Item String]
loadContent p = recentFirst =<< loadAllSnapshots p "content"

-- | Replaces @href="/foo/index.html"@ with @href="/foo"@
replaceIndexLinks :: Item String -> Compiler (Item String)
replaceIndexLinks = replace "href=\"/[^\"]*/index.html" P.takeDirectory

-- | Replaces @<host>/foo/index.html@ with @<host>/foo@ for the given host
replaceIndexURLs :: String -> Item String -> Compiler (Item String)
replaceIndexURLs host = replace (host <> "/.*/index.html") P.takeDirectory

replace
    :: String             -- ^ Regular expression to match
    -> (String -> String) -- ^ Provide replacement given match
    -> Item String -> Compiler (Item String)
replace p f = return . fmap (replaceAll p f)
