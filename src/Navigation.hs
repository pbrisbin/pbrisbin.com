-- |
--
-- A construct for showing next/previous links in templates
--
module Navigation
    ( Page(..)
    , Navigation
    , buildNavigation
    , nextUrlField
    , prevUrlField
    ) where

import Hakyll

import Control.Applicative ((<$>))
import Data.List (sort)

import qualified Data.Map as M

data Page = Page
    { nextIdentifier :: Maybe Identifier
    , prevIdentifier :: Maybe Identifier
    }

type Direction = Page -> Maybe Identifier
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
nextUrlField key nav = field key $
    getPageUrl nextIdentifier nav . itemIdentifier

-- | Add the previous identifier's url at the given key
prevUrlField :: String -> Navigation -> Context String
prevUrlField key nav = field key $
    getPageUrl prevIdentifier nav . itemIdentifier

getPageUrl :: Direction -> Navigation -> Identifier -> Compiler String
getPageUrl direction nav current =
    renderUrl $ direction =<< M.lookup current nav

  where
    renderUrl (Just i) = maybe "/" toUrl <$> getRoute i
    renderUrl Nothing = return "/"
