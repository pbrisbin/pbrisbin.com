---
title: "Phantom Types and Globbing Bugs"
tags: haskell, restyled
---

I love concrete examples that illustrate the day-to-day of the professional
Haskell programmer, and show the inspiration for that entirely-untrue quip, "if
it compiles, it works". What _is_ true is some variation of "if it compiles, any
logic encoded in the types is correct". That statement is a tautology (if a
program type checks, it means the types are correct), but "encoding logic in the
types" isn't an activity that happens much outside of Haskell.

So let's talk about it.

### Lay of the land

Let's begin at the beginning; the code as it was before I started:

```hs
-- Restyler.Config
data Config = Config
  { -- ..
  , cIgnoreLabels :: Set (Name IssueLabel)
  }
```

```hs
-- Restyler.Main
someMainLikeFunction = do
  -- ...

  labels <- getPullRequestLabels pullRequest

  when (labels `intersect` cIgnoreLabels config) $ do
    -- Some cleanup and then exit

  -- ...
```

This is in the [restyler][] CLI, part of my [Restyled][] project. It's in charge
of skipping over Pull Requests that have certain labels. The `Config` type is
read from a Yaml configuration file where these behaviors can be specified by
the user in the repository itself. The feature I was working on was to extend
this to support ignoring by author or branch, and to match everything as
"globs". The main motivation was to support ignoring Pull Requests with branches
like `renovate/*` or authors like `*[bot]`.

[restyler]: https://github.com/restyled-io/restyler
[restyled]: https://restyled.io

### Make the change easy

Before making any extensions to something, I immediately consider extracting
that thing to a new module. Selfishly, I like to carve out my own green-field
workspace, but there are also more objective benefits such as a cleaner diff,
establishing bounded contexts, and defining a unit to be tested. You can't do
that when your logic is peppered in the middle of `someMainLikeFunction`, but
you can with `getIgnoredReason`:

```hs
-- Restyler.Main
someMainLikeFunction = do
  -- ...

  mReason <- getIgnoredReason config pullRequest

  for_ mReason $ \reason -> do
    -- Some cleanup and then exit

  -- ...
```

```hs
-- Restyler.Ignore
data IgnoredReason
  = IgnoredByLabels

getIgnoredReason :: Config -> PullRequest -> m (Maybe IgnoredReason)
getIgnoredReason Config {..} pullRequest = do
  labels <- getPullRequestLabels pullRequest

  pure $ asum
    [ IgnoredByLabels <$ guard (labels `intersect` cIgnoreLabels)
    ]
```

This `asum` construction is pretty awkward, but I knew the other conditions were
coming and having this list, ready to be extended, would make such a change,
well, easy.

### Glob

I chose to add glob support to the existing labels logic before adding author
and branch as ignored-reasons. Restyled uses globs in the `exclude`/`include`
options on `FilePath`s, so we had this code already:

```hs
module Restyler.Config.Glob
  ( Glob
  , match
  ) where

import System.FilePath.Glob as Glob

newtype Glob = Glob Pattern

instance FromJSON Glob where
  -- ...

match :: Glob -> FilePath -> Bool
match (Glob p) = Glob.match p
```

The idea for this module was to encapsulate our use of the `Glob` library. Not
for any important reason, mostly so I could give it `FromJSON` without an
orphan.

Anyway, let's extend it for labels, which are a few `newtype`s away from `Text`:

```hs
-- Restyler.Config.Glob
matchText :: Glob -> Text -> Bool
matchText g = match g . unpack
```

```diff
 -- Restyler.Config
 data Config = Config
   { -- ...
-  , cIgnoreLabels :: Set (Name IssueLabel)
+  , cIgnoreLabels :: [Glob]
   }
```

```diff
 -- Restyler.Ignore
 getIgnoredReason :: Config -> PullRequest -> m (Maybe IgnoredReason)
 getIgnoredReason Config {..} pullRequest = do
   labels <- getPullRequestLabels pullRequest
 
   pure $ asum
-    [ IgnoredByLabels <$ guard (cIgnoreLabels `intersect` labels)
+    [ IgnoredByLabels <$ guard (cIgnoreLabels `matchAny` (toPathPart <$> labels))
     ]
```

```hs
matchAny :: Foldable t => [Glob] -> t [Text] -> Bool
matchAny globs = any $ \t -> any (`matchText` t) globs
```

Why not put `matchAny` in the `Glob` module? It seems like a good fit, but I
hesitated because of naming hiccups. `match` is for `FilePath`, `matchText` is
for `Text`, so this function on `[Text]` should be `matchAnyText`?
`matchTextAny`? Do I need to define a `[FilePath]` version too? This uncertainty
is good feedback. What it's trying to tell us will come back around later. In
the face of uncertainty like this, I always leave things defined right where
they're used, so an incorrect abstraction doesn't have a chance to take root.

### Make the easy change

```diff
 data Config = Config
   { -- ...
   , cIgnoreLabels :: [Glob]
+  , cIgnoreAuthors :: [Glob]
+  , cIgnoreBranches :: [Glob]
   }
```

```diff
 data IgnoredReason
   = IgnoredByLabels
+  | IgnoredByAuthor
+  | IgnoredByBranch
 
 getIgnoredReason :: Config -> PullRequest -> m (Maybe IgnoredReason)
 getIgnoredReason Config {..} pullRequest = do
   labels <- getPullRequestLabels pullRequest
 
   pure $ asum
     [ IgnoredByLabels <$ cIgnoreLabels `matchAny` (toPathPart <$> labels)
+    , IgnoredByAuthor <$ cIgnoreAuthors `matchAny` [toPathPart author]
+    , IgnoredByBranch <$ cIgnoreLabels `matchAny` [branch]
     ]
+  where
+    author = pullRequestUserName pullRequest
+    branch = pullRequestBaseRef pullRequest
```

Ship it? It compiles; the tests pass too. I figured most of the complexity was
in `matchAny`, so that was well-covered, and I tested the label ignoring
thoroughly when that was added; surely this is just more of the same and doesn't
require additional, dedicated test coverage. Right?

### Types are a lie

The bug may be obvious to you when presented this way, but it is typical of the
kind of logic errors (simple) types can't save you from: `cIgnoreLabels` and
`cIgnoreBranches` are the same type, so the code compiles just fine with this
bug present.

```hs
    , IgnoredByBranch <$ cIgnoreLabels `matchAny` [branch]
    --                          ^ copy pasta fail
```

How did Haskell let this happen?

<div class="centered">
![](https://images.pbrisbin.com/phantom_types_and_globbing_bugs/haskell-bike.jpg)
</div>

We're using `Glob` in 4 different cases:

1. Ignore Labels matched against `Name IssueLabel`s
1. Ignore Authors matched against `Name User`s
1. Ignore Branches matched against `Text`
1. Excludes (original use) matched against `FilePath`s

The reason types didn't save us is we didn't express this with them, we just
converted everything to `Text`, like shoving a stick in our bike wheel.

### Phantom type variables

Phantom type variables are a great trick when two otherwise equivalent values
need to be distinguished at the type level so you don't mix them up. Let's see
it with Glob:

```diff
 -- Restyler.Config.Glob
-data Glob = Glob Pattern
+data Glob a = Glob Pattern
 
-match :: Glob -> FilePath -> Bool
+match :: Glob a -> FilePath -> Bool
 match (Glob p) = Glob.match p
```

The type variable isn't used in the actual type (hence, phantom). Since it has
no bearing on anything, our `match` function works the same as before. But what
we can do now is specify one `a` for some globs and a different `a` for others,
so we can't mix them up.

```diff
 --- Restyler.Config
 data Config = Config
   { -- ...
-  , cIgnoreLabels :: [Glob]
-  , cIgnoreAuthors :: [Glob]
-  , cIgnoreBranches :: [Glob]
+  , cIgnoreLabels :: [Glob (Name IssueLabel)]
+  , cIgnoreAuthors :: [Glob (Name User)]
+  , cIgnoreBranches :: [Glob Text]
   }
```

So clear!

But wait, the code with the bug still compiles:

```hs
    [ IgnoredByLabels <$ cIgnoreLabels `matchAny` (toPathPart <$> labels)
    , IgnoredByAuthor <$ cIgnoreAuthors `matchAny` [toPathPart author]
    , IgnoredByBranch <$ cIgnoreLabels `matchAny` [branch]
    ]
```

How can that be? Well, `matchAny` (built on `matchText`, built on `match`) makes
no demands of the `a` so it works with any `Glob`. Let's fix that...

```hs
-- Restyler.Config.Glob
data Glob a = Glob Pattern

class GlobTarget a where
  forMatch :: a -> String

instance GlobTarget FilePath where
  forMatch = id

instance GlobTarget Text where
  forMatch = unpack

instance GlobTarget (Name a) where
  forMatch = forMatch . toPathPart

match :: GlobTarget a => Glob a -> a -> Bool
match (Glob p) = Glob.match p . forMatch

matchAny :: (Foldable t, GlobTarget a) => [Glob a] -> t a -> Bool
matchAny globs = any $ \a -> any (`match` a) globs
```

So many things have snapped into place here:

First of all, our bugged code fails to compile now. Since the `Glob a` has to
agree with the `a` you call it on, it's impossible to misuse in the way we did:

```
    • Couldn't match expected type ‘Name IssueLabel’
                  with actual type ‘Text’
    • In the expression: branch
      In the second argument of ‘matchAny’, namely ‘[branch]’
      In the first argument of ‘guard’, namely
        ‘(cIgnoreLabels `matchAny` [branch])’
   |
42 |     , IgnoredByBranch <$ guard (cIgnoreLabels `matchAny` [branch])
   |
```

Second, `matchAny` now makes perfect sense in the `Glob` module because there's
no need for this weird `FilePath`/`Text` naming confusion, it really is just an
`any`-fied version of `match` now.

And finally, something I find absolutely _wild_, our pre-existing use of `Glob`
for `FilePath`s in the `include`/`exclude` never had to change. The only thing I
had to touch was when the type variable was introduced:

```diff
 data Config = Config
-  { cExcludes :: [Glob]
+  { cExcludes :: [Glob FilePath]
   , -- ...
   }
```

Which I think is a great boost to clarity there too.

And I guess we should finally fix our bug:

```diff
    [ IgnoredByLabels <$ cIgnoreLabels `matchAny` (toPathPart <$> labels)
    , IgnoredByAuthor <$ cIgnoreAuthors `matchAny` [toPathPart author]
-   , IgnoredByBranch <$ cIgnoreLabels `matchAny` [branch]
+   , IgnoredByBranch <$ cIgnoreBranches `matchAny` [branch]
    ]
```

If you're interested in seeing this work where it really happened, you can check
out the [Pull Request][pr]. Note that some details have been changed for this
post.

[pr]: https://github.com/restyled-io/restyler/pull/141
