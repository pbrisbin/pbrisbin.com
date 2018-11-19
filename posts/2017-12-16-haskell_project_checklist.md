---
title: "Haskell Project Checklist"
tags: haskell
---

The following are all the things I want in place for a Haskell project. This is
primarily a copy-paste-able reference for myself, but I've also tried to explain
or generalize some things to make it useful for anyone first bootstrapping a
Haskell project.

<div class="well">
**NOTE**: if you were brought here after googling something like "how to Haskell
on Circle 2.0", you'll just need the [`Makefile`](#makefile) and
[`.circleci/config.yml`](#circleyml).
</div>

## 1. Use [stack][] & [hpack][]

[stack]: https://docs.haskellstack.org/en/stable/README/
[hpack]: https://github.com/sol/hpack#readme

**.gitignore**

```gitignore
*.cabal
.stack-work
```

**stack.yaml**

```yaml
---
resolver: lts-12.19

ghc-options:
  "$locals": -fhide-source-paths
```

**package.yaml**

```yaml
---
name: {package-name}
version: 0.0.0.0  # EPOCH.MAJOR.MINOR.PATCH
category:
synopsis: Short synopsis
description: >
  Longer, wrapping description.
author:
maintainer:
github: {username}/{package-name}
license: MIT

ghc-options: -Wall

dependencies:
  - base >=4.8.0 && <5  # GHC 7.10+

library:
  source-dirs: src
  dependencies:
    - text  # for example

tests:
  # More on this later
```

## 2. Run Everything Through `make`

**Makefile**

<div id=makefile>
```makefile
all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: test
test:
	stack test

.PHONY: lint
lint:
	hlint .
	weeder .
```
</div>

## 3. Use Hspec

**package.yaml**

```yaml
tests:
  spec:
    main: Spec.hs
    source-dirs: test
    dependencies:
      - {package-name}
      - hspec
```

**test/Spec.hs**

```hs
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

Add modules that export a `spec :: Spec` function and match `test/**/*Spec.hs`.

## 4. Use Doctest

**package.yaml**

```yaml
tests:
  spec:
    # ...

  doctest:
    main: Main.hs
    source-dirs: doctest
    dependencies:
      - doctest
```

**doctest/Main.hs**

```hs
module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest ["-XOverloadedStrings", "src"]
```

Fill your Haddocks with executable examples.

```hs
-- | Strip whitespace from the end of a string
--
-- >>> stripEnd "foo  "
-- "foo"
--
stripEnd :: String -> String
stripEnd = -- ...
```

See the [Doctest documentation][doctest] for more details.

[doctest]: https://github.com/sol/doctest#readme
[doctest-discover]: https://github.com/karun012/doctest-discover#readme
[doctest-discover-issue]: https://github.com/karun012/doctest-discover/issues/9

## 5. Always Be Linting

As you saw, we have a `make lint` target that uses HLint and Weeder. I also have
my editor configured to run `stylish-haskell` on write.

**.hlint.yaml**

```yaml
---
- ignore:
    name: Redundant do
    within: spec
```

**.stylish-haskell.yaml**

<div class="well">
**WARNING**: opinionated!
</div>

```yaml
---
steps:
  - simple_align:
      cases: false
      top_level_patterns: false
      records: false
  - imports:
      align: none
      list_align: after_alias
      pad_module_names: false
      long_list_align: new_line_multiline
      empty_list_align: right_after
      list_padding: 4
      separate_lists: false
      space_surround: false
  - language_pragmas:
      style: vertical
      align: false
      remove_redundant: true
  - trailing_whitespace: {}
columns: 80
newline: native
```

The defaults for `weeder` are usually fine for me.

<div class="well">
If you're interested in having style fixes automatically resolved as part of
your Pull Request process, check out [Restyled](https://restyled.io).
</div>

## 6. Use Circle 2.0

When you set up the project, make sure you say it's Haskell via the *Other*
option in the language select; maybe they'll add better support in the future.

**.circleci/config.yml**

<div id=circleyml>
```yaml
---
version: 2.0

jobs:
  build:
    docker:
      - image: fpco/stack-build:lts-9.18
    steps:
      - checkout
      - run:
          name: Digest
          command: |
            # Bust cache on any tracked file changing. We'll still fall back to
            # the most recent cache for this branch, or master though.
            git ls-files | xargs md5sum > digest

      - restore_cache:
          keys:
            - stack-{{ .Branch }}-{{ checksum "digest" }}
            - stack-{{ .Branch }}-
            - stack-master-
            - stack-
      - run:
          name: Dependencies
          command: make setup
      - run:
          name: Build
          command: make build
      - save_cache:
          key: stack-{{ .Branch }}-{{ checksum "digest" }}
          paths:
            - ~/.stack
            - ./.stack-work
      - run:
          name: Test
          command: make test
      - run:
          name: Lint
          command: make lint
```
</div>

![Haskell CI Example](https://images.pbrisbin.com/haskell_project_checklist/ci-example.png)\ 

Quite nice.

Don't forget to enable "build forked Pull Requests" in Circle's settings.

## 7. Release to Hackage

I wrap this up in my own [hackage-release script][hackage-release], but here are
the relevant actions:

[hackage-release]: https://github.com/pbrisbin/dotfiles/blob/master/local/bin/hackage-release

```console
stack build --pedantic --test
stack upload .
```

And it's a good practice to tag releases:

```console
git tag --sign --message "v$version" "v$version"
git push --follow-tags
```

## 8. Add to Stackage

Check the documentation [here][stackage-maintainers]. In short, just open a Pull
Request adding yourself and/or your package to
[`build-constraints.yaml`][build-constraints]. It can be done without even
leaving GitHub.

[stackage-maintainers]: https://github.com/fpco/stackage/blob/master/MAINTAINERS.md#adding-a-package
[build-constraints]: https://github.com/fpco/stackage/blob/master/build-constraints.yaml

You should ensure your package builds "on nightly". I add a target for this to
my `Makefile`:

```makefile
.PHONY: check-nightly
check-nightly:
	stack setup --resolver nightly
	stack build --resolver nightly --pedantic --test
```

Sometimes I have this run on CI, sometimes I don't.
