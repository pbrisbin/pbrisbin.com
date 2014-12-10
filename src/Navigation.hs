-- |
--
-- A construct for showing next/previous links in templates
--
-- Example usage:
--
-- In @main.hs@:
--
-- > navigation <- buildNavigation "posts/*"
-- >
-- > match "posts/*" $ do
-- >     route idRoute
-- >     compile $ do
-- >         let ctx = mconcat
-- >                 [ nextUrlField "next" navigation
-- >                 , prevUrlField "prev" navigation
-- >                 , nextTitleField "nextTitle" navigation
-- >                 , prevTitleField "prevTitle" navigation
-- >                 , defaultContext
-- >                 ]
-- >
-- >         pandocCompiler
-- >             >>= loadAndApplyTemplate "templates/post.html" ctx
--
-- And in @templates/post.html@:
--
-- > <ul class="pager">
-- >   $if(prev)$
-- >     <li class="previous">
-- >       <a href="$prev$">⇐ $prevTitle$</a>
-- >     </li>
-- >   $endif$
-- >
-- >   $if(next)$
-- >     <li class="next">
-- >       <a href="$next$">$nextTitle$ ⇒</a>
-- >     </li>
-- >   $endif$
-- > </ul>
--
module Navigation
    ( Page(..)
    , Navigation
    , buildNavigation
    , buildNavigationWith
    , nextUrlField
    , nextTitleField
    , prevUrlField
    , prevTitleField
    ) where

import Hakyll

import Control.Applicative (Alternative(..), (<$>))
import Control.Monad ((<=<))
import Data.List (sort)

import qualified Data.Map as M

data Page = Page
    { nextIdentifier :: Maybe Identifier -- ^ @Nothing@ for last page
    , prevIdentifier :: Maybe Identifier -- ^ @Nothing@ for first page
    }

type Direction = Page -> Maybe Identifier

-- | Each page mapped to its next/previous identifiers
type Navigation = M.Map Identifier Page

-- | Build a @Navigation@ for the given @Pattern@
--
-- This function determines next/previous by position in a simple @sort@ of the
-- matched idientifiers. See @buildNavigationWith@ to provide something else.
--
buildNavigation :: MonadMetadata m => Pattern -> m Navigation
buildNavigation = buildNavigationWith sort

buildNavigationWith :: MonadMetadata m
                    => ([Identifier] -> [Identifier])
                    -> Pattern
                    -> m Navigation
buildNavigationWith order pattern =
    -- s/b fmap but MonadMetadata is not currently a Functor
    return . go Nothing M.empty . order =<< getMatches pattern

  where
    go :: Maybe Identifier -> Navigation -> [Identifier] -> Navigation
    go _ nav [] = nav
    go p nav [x] = M.insert x (Page p Nothing) nav
    go p nav (x:y:rest) =
        go (Just x) (M.insert x (Page p (Just y)) nav) (y:rest)

-- | Add the next identifier's url at the given key
nextUrlField :: String -> Navigation -> Context String
nextUrlField key = maybeField key . getPageUrl nextIdentifier

-- | Add the next identifier's title at the given key
nextTitleField :: String -> Navigation -> Context String
nextTitleField key = maybeField key . getPageTitle nextIdentifier

-- | Add the previous identifier's url at the given key
prevUrlField :: String -> Navigation -> Context String
prevUrlField key = maybeField key . getPageUrl prevIdentifier

-- | Add the previous identifier's title at the given key
prevTitleField :: String -> Navigation -> Context String
prevTitleField key = maybeField key . getPageTitle prevIdentifier

getPageUrl :: Direction -> Navigation -> Item a -> Compiler (Maybe String)
getPageUrl = withPage $ \page -> do
    mr <- getRoute page
    return $ toUrl <$> mr

getPageTitle :: Direction -> Navigation -> Item a -> Compiler (Maybe String)
getPageTitle = withPage $ \page -> do
    md <- getMetadata page
    return $ M.lookup "title" md

withPage :: (Identifier -> Compiler a)
         -> Direction
         -> Navigation
         -> Item b
         -> Compiler a
withPage f direction nav item =
    maybe empty f $ direction =<< M.lookup (itemIdentifier item) nav

-- https://github.com/jaspervdj/hakyll/pull/311
maybeField :: String -> (Item a -> Compiler (Maybe String)) -> Context a
maybeField key value = field key $ maybe empty return <=< value
