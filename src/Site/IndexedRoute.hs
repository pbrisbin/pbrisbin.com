module Site.IndexedRoute
    ( indexedRoute
    , replaceIndexLinks
    , replaceIndexURLs
    ) where

import           Hakyll

import           Data.Monoid           ((<>))
import           System.FilePath       (splitFileName, (</>))

import qualified System.FilePath.Posix as P

indexedRoute :: Routes
indexedRoute = customRoute $ \i ->
    let (path, name) = splitFileName $ toFilePath i
    in path </> dropDatePrefix name </> "index.html"

  where
    dropDatePrefix = drop 11

-- | Replaces @href="/foo/index.html"@ with @href="/foo"@
replaceIndexLinks :: Item String -> Compiler (Item String)
replaceIndexLinks = replace "href=\"/[^\"]*/index.html" P.takeDirectory

-- | Replaces @<host>/foo/index.html@ with @<host>/foo@ for the given host
replaceIndexURLs :: String -> Item String -> Compiler (Item String)
replaceIndexURLs host = replace (host <> "/.*/index.html") P.takeDirectory

replace :: String             -- ^ Regular expression to match
        -> (String -> String) -- ^ Provide replacement given match
        -> Item String
        -> Compiler (Item String)
replace p f = return . fmap (replaceAll p f)
