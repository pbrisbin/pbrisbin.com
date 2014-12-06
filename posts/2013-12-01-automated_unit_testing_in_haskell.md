---
title: Automated Unit Testing in Haskell
tags: testing, haskell, cabal, hunit, ruby, guard
---

[Hspec][] is a [BDD][] library for writing Rspec-style tests in Haskell. 
In this post, I'm going to describe setting up a Haskell project using 
this test framework. What we'll end up with is a series of tests which 
can be run individually (at the module level), or all together (as part 
of packaging). Then I'll briefly mention [Guard][] (a Ruby tool) and how 
we can use that to automatically run relevant tests as we change code.

[hspec]: http://hspec.github.io/

[bdd]: http://en.wikipedia.org/wiki/Behavior-driven_development

[guard]: https://github.com/guard/guard

## Project Layout

For any of this to work, our implementation and test modules must follow 
a particular layout:

```
Code/liquid/
├── src
│   └── Text
│       ├── Liquid
│       │   ├── Context.hs
│       │   ├── Parse.hs
│       │   └── Render.hs
│       └── Liquid.hs
└── test
    ├── SpecHelper.hs
    ├── Spec.hs
    └── Text
        └── Liquid
            ├── ParseSpec.hs
            └── RenderSpec.hs
```

Notice that for each implementation module (under `./src`) there is a 
corresponding spec file at the same relative path (under `./test`) with 
a consistent, conventional name (`<ModuleName>Spec.hs`). For this post, 
I'm going to outline the first few steps of building the `Parse` module 
of the above source tree which happens to be my [liquid][] library, a 
Haskell implementation of Shopify's [template][] system.

[liquid]: https://github.com/pbrisbin/liquid

[template]: http://liquidmarkup.org/

## Hspec Discover

Hspec provides a useful function called `hspec-discover`. If your 
project follows the conventional layout above, you can simply create a 
file like so:

**test/Spec.hs**

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

And when that file is executed, all of your specs will be found and run 
together as a single suite.

## SpecHelper

I like to create a central helper module which gets imported into all 
specs. It simply exports our test framework and implementation code:

**test/SpecHelper.hs**

```haskell
module SpecHelper
    ( module Test.Hspec
    , module Text.Liquid.Parse
    ) where

import Test.Hspec
import Text.Liquid.Parse
```

This file might not seem worth it now, but as you add more modules, it 
becomes useful quickly.

## Baby's First Spec

**test/Text/Liquid/ParseSpec.hs**

```haskell
module Text.Liquid.ParseSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Text.Liquid.Parse" $ do
        context "Simple text" $ do
            it "parses exactly as-is" $ do
                let content = "Some simple text"

                parseTemplate content `shouldBe` Right [TString content]

main :: IO ()
main = hspec spec

```

With this first spec, I've already made some assumptions and design 
decisions.

The API into our module will be a single `parseTemplate` function which 
returns an `Either` type (commonly used to represent success or 
failure). The `Right` value (conventionally used for success) will be a 
list of template parts. One such part can be constructed with the 
`TString` function and is used to represent literal text with no 
interpolation or logic. This is the simplest template part possible and 
is therefore a good place to start.

The `spec` function is what will be found by `hspec-discover` and rolled 
up into a project-wide test. I've also added a `main` function which 
just runs said `spec`. This allows me to easily run the spec in 
isolation, which you should do now:

```
$ runhaskell -isrc -itest test/Text/Liquid/ParseSpec.hs
```

The first error you should see is an inability to find `Test.Hspec`. Go 
ahead and install it:

```
$ cabal install hspec
```

You should then get a similar error for `Text.Liquid.Parse` then some 
more about functions and types that are not yet defined. Let's go ahead 
and implement just enough to get past that:

**src/Text/Liquid/Parse.hs**

```haskell
module Text.Liquid.Parse where

type Template = [TPart]

data TPart = TString String

parseTemplate :: String -> Either Template String
parseTemplate = undefined
```

The test should run now and give you a nice red failure due to the 
attempted evaluation of `undefined`.

Since implementing `Parse` is not the purpose of this post, I won't be 
moving forward in that direction. Instead, I'm going to show you how to 
set this library up as a package which can be `cabal install`ed and/or 
`cabal test`ed by end-users.

For now, you can pass the test easily like so:

**src/Text/Liquid/Parse.hs**

```haskell
parseTemplate :: String -> Either Template String
parseTemplate str = Right [TString str]
```

For TDD purists, this is actually the correct thing to do here: write 
the simplest implementation to pass the test (even if you "know" it's 
not going to last), then write another failing test to force you to 
implement a little more. I don't typically subscribe to that level of 
TDD purity, but I can see the appeal.

## Cabal

We've already got `Spec.hs` which, when executed, will run all our specs 
together:

```
$ runhaskell -isrc -itest test/Spec.hs
```

We just need to wire that into the Cabal packaging system:

**liquid.cabal**

```
name:          liquid
version:       0.0.0
license:       MIT
copyright:     (c) 2013 Pat Brisbin
author:        Pat Brisbin <pbrisbin@gmail.com>
maintainer:    Pat Brisbin <pbrisbin@gmail.com>
build-type:    Simple
cabal-version: >= 1.8

library
  hs-source-dirs: src

  exposed-modules: Text.Liquid.Parse

  build-depends: base == 4.*

test-suite spec
  type: exitcode-stdio-1.0

  hs-source-dirs: test

  main-is: Spec.hs

  build-depends: base  == 4.*
               , hspec >= 1.3
               , liquid
```

With this in place, testing our package is simple:

```
$ cabal configure --enable-tests
...
$ cabal build
...
$ cabal test
Building liquid-0.0.0...
Preprocessing library liquid-0.0.0...
In-place registering liquid-0.0.0...
Preprocessing test suite 'spec' for liquid-0.0.0...
Linking dist/build/spec/spec ...
Running 1 test suites...
Test suite spec: RUNNING...
Test suite spec: PASS
Test suite logged to: dist/test/liquid-0.0.0-spec.log
1 of 1 test suites (1 of 1 test cases) passed.
```

## Guard

Another thing I like to setup is the automatic running of relevant specs 
as I change code. To do this, we can use a tool from Ruby-land called 
[Guard][]. Guard is a great example of a simple tool doing one thing 
well. All it does is watch files and execute actions based on rules 
defined in a `Guardfile`. Through plugins and extensions, there are a 
number of pre-built solutions for all sorts of common needs: restarting 
servers, regenerating ctags, or running tests.

We're going to use [guard-shell][] which is a simple extension allowing 
for running shell commands and spawning notifications.

[guard-shell]: https://github.com/guard/guard-shell

```
$ gem install guard-shell
```

Next, create a `Guardfile`:

**Guardfile**

```ruby
# Runs the command and prints a notification
def execute(cmd)
  if system(cmd)
    n 'Build succeeded', 'hspec', :success
  else
    n 'Build failed', 'hspec', :failed
  end
end

def run_all_tests
  execute %{
    cabal configure --enable-tests &&
    cabal build && cabal test
  }
end

def run_tests(mod)
  specfile = "test/#{mod}Spec.hs"

  if File.exists?(specfile)
    files = [specfile]
  else
    files = Dir['test/**/*.hs']
  end

  execute "ghc -isrc -itest -e main #{files.join(' ')}"
end

guard :shell do
  watch(%r{.*\.cabal$})          { run_all_tests }
  watch(%r{test/SpecHelper.hs$}) { run_all_tests }
  watch(%r{src/(.+)\.hs$})       { |m| run_tests(m[1]) }
  watch(%r{test/(.+)Spec\.hs$})  { |m| run_tests(m[1]) }
end
```

Much of this `Guardfile` comes from [this][] blog post by Michael 
Xavier. His version also includes cabal sandbox support, so be sure to 
check it out if that interests you.

<div class="well">
If you like to bundle all your Ruby gems (and you probably should) that 
can be done easily, just see my main [liquid][] repo as that's how I do 
things there.
</div>

[this]: http://www.michaelxavier.net/posts/2013-07-07-Rakefile-for-Developing-Cabal-Projects-in-Haskell.html

In one terminal, start guard:

```
$ guard
```

Finally, simulate an edit in your module and watch the test 
automatically run:

```
$ touch src/Text/Liquid/Parse.hs
```

And there you go, fully automated unit testing in Haskell.
