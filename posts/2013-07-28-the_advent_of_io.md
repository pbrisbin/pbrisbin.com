---
title: The Advent of IO
tags: haskell, monad, io
---

What if we wanted to write a Haskell program to behave something like 
this:

```
$ runhaskell hello.hs
Hello who?

$ runhaskell hello.hs Pat
Hello Pat

$ runhaskell hello.hs -u Pat
Hello PAT
```

One implementation may look like this:

```haskell 
main :: IO ()
main = do
    args <- getArgs

    let name = case args of
                ("-u":n:_) -> map toUpper n
                (     n:_) -> n
                otherwise  -> "who?"

    putStrLn $ "Hello " ++ name
```

And almost immediately, the budding Haskell programmer is met with a 
number of confusing concepts: What the heck is `IO ()`? What does `<-` 
mean? When questions like these are raised, the answer is "well, because 
Monad." Not very enlightening.

Haskell's IO monad is an amazingly elegant solution to a very thorny 
problem, but why is it so hard to wrap one's head around? I think the 
reason it can be so confusing is that we come at it backwards, we see 
this elegant result but know not the problem it solves.

## In the Beginning

In the very early days of Haskell, there was no `IO` monad. Instead, 
programs used a somewhat confusing `[Response] -> [Request]` model (some 
details can be found [here]).

[here]: http://stackoverflow.com/a/17004448

It was clear that if Haskell were to become generally useful, there had 
to be something better, something that allowed more intuitive 
interactions with the outside word. The problem was extending this idea 
of a globally accessible Outside World without sacrificing the purity of 
the program.

Recently, while pondering the `State` monad, I had an epiphany which 
confirms how the problem was solved: **Every function is still pure**.

How is this possible? Well, first we have to look at IO actions as any 
other form of stateful computation. Then we just have to prove to 
ourselves that stateful computations can be done in a pure way.

Take a program like this:

```haskell 
main :: IO ()
main = doTheThing

doTheThing :: IO ()
doTheThing = do
    putStrLn "one"
    putStrLn "two"
```

It's common to refer to these functions as impure and having side 
effects. We look at an imperative line like `putStrLn` and assume that 
the function is "reaching out" affecting the outside world by printing 
text to some terminal it has not received as a direct input, and is 
therefore impure.

This mis-characterization isn't itself bad, we do need a way to 
differentiate Haskell functions which "live in IO" vs those that don't. 
Pure vs impure seems like good enough categories, but it's not entirely 
correct and can lead folks astray when more complex concepts are 
introduced.

Imagine if we instead wrote the program like this:

```haskell 
main :: World -> (World, ())
main world = doTheThing world

putStrLn :: String -> World -> (World, ())
putStrLn str world = appendText (str ++ "\n") (terminal world)

doTheThing :: World -> (World, ())
doTheThing world =
    let (world1, _) = (putStrLn "one") world
        (world2, _) = (putStrLn "two") world1

    in (world2, ())
```

I've purposely left `appendText` undefined and not told you what `World` 
is, but you can still confirm that these functions act only on their 
direct inputs, thus remaining completely pure. If we accept that there 
is some notion of a `World` to which we can `appendText` provided by the 
Haskell language, then the above is a completely accurate de-sugaring of 
the original program.

To further explore this idea, I went through the mental exercise of 
building the IO monad myself by substituting my own `World` into the 
confines of a very simple alternate `main` syntax.

I hope you'll find it as illustrative as I did.

## Limiting Main.main

Let's pretend that Haskell is in its infancy and the designers have 
punted the idea of IO. They've chosen instead to flesh out the rest of 
the language with vastly simpler semantics for a program's `main`.

In this hypothetical language, a program's `main` function is of the 
type `[String] -> String`. When executed, the Haskell runtime will 
provide the program's commandline arguments to your `main` function as a 
list of `String`s. Whatever `String` your `main` function returns will 
then be printed on `stdout`.

Let's try out this language on our sample problem:

```haskell 
import Data.Char (toUpper)

main1 :: [String] -> String
main1 args = sayHello1 args

sayHello1 :: [String] -> String
sayHello1 args = "Hello " ++ (nameFromArgs1 args)

nameFromArgs1 :: [String] -> String
nameFromArgs1 ("-u":name:_) = map toUpper name
nameFromArgs1 (     name:_) = name
nameFromArgs1            _  = "who?"
```

Obviously things could be done simpler, but I've purposely written it 
using two functions: one which requires access to program input and one 
which affects program output. This will make our exercise much more 
interesting as we move toward monadic IO.

Our current method of passing everything that's needed as direct 
arguments and getting back anything that's needed as direct results 
works well for simple cases, but it doesn't scale. When we consider that 
the input to and output of `main` might eventually be a rich object 
representing the entire outside world (file handles, TCP sockets, 
environment variables, etc), it becomes clear that passing these 
resources down into and back out of any functions we wish to use is 
simply not workable.

However, passing the data directly in and getting the result directly 
out is the only way to keep functions pure. It's also the only way to 
keep them honest. If any one function needs access to some piece of the 
outside world, any functions which use it also need that same access. 
This required access propagates all the way up to `main` which is the 
only place that data is available a-priori.

What if there were a way to continue to do this but simply make it 
easier on the eyes (and fingers) through syntax or abstraction?

## Worldly Actions

The solution to our problem begins by defining two new types: `World` 
and `Action`.

A `World` is just something that represents the commandline arguments 
given to `main` and the `String` which must be returned by `main` for 
our program to have any output. At this point in time, there's no other 
aspects of the world that we have access to or could hope to affect.

```haskell 
data World = World
    { input  :: [String]
    , output :: String
    }
```

An `Action` is a function which takes one `World` and returns a different one
along with some result. The differences between the given `World` and the
returned one are known as the function's side-effects. Often, we don't care
about the result itself and only want the side-effects, in these cases we'll use
Haskell's `()` (known as *Unit*) as the result.

```haskell 
sayHello2 :: World -> (World, ())
sayHello2 w =
    let (w', n) = nameFromArgs2 w

    in (w' { output = output w ++ "Hello " ++ n }, ())

nameFromArgs2 :: World -> (World, String)
nameFromArgs2 w =
    case input w of
        ("-u":name:_) -> (w, map toUpper name)
        (     name:_) -> (w, name)
        otherwise     -> (w, "who?")
```

Now we can rewrite `main` to just convert its input and output into a 
`World` which gets passed through our world-changing functions.

```haskell 
main2 :: [String] -> String
main2 args =
    let firstWorld    = World args ""
        (newWorld, _) = sayHello2 firstWorld

    in output newWorld
```

In the above, we've just accepted that `World -> (World, a)` is this 
thing we call an `Action`. There's no reason to be implicit about these 
things in Haskell, so let's give it a name.

```haskell 
newtype Action w a = Action { runAction :: (w -> (w, a)) }
```

In order to create a value of this type, we simply need to give a 
world-changing function to its constructor. The `runAction` accessor 
allows us to pull the actual world-changing function back out again. 
Once we have the function itself, we can execute it on any value of type 
`w` and we'll get a new value of type `w` along with a result of type 
`a`.

As mentioned, we often don't care about the result and want to run an 
`Action` only for its side-effects. This next function makes running an 
action and discarding its result easy:

```haskell 
execAction :: Action w a -> w -> w
execAction a w = let (w', _) = (runAction a) w in w'
```

This becomes immediately useful in our newest `main`:

```haskell 
main3 :: [String] -> String
main3 args = output $ execAction (Action sayHello2) (World args "")
```

You'll notice we need to pass `sayHello2` to the `Action` constructor 
before giving it to `execAction`. This is because `sayHello2` is just 
the world-changing function itself. For reasons that should become clear 
soon, we don't want to do this, it would be better for our 
world-changing functions to be actual `Action`s themselves.

Before we address that, let's define a few helper `Action`s:

```haskell 
-- | Access a world's input without changing it
getArgs :: Action World [String]
getArgs = Action (\w -> (w, input w))

-- | Change a world by appending str to its output buffer
putStrLn :: String -> (Action World ())
putStrLn str = Action (\w ->
    (w { output = (output w) ++ str ++ "\n"}, ()))
```

Now let's fix our program:

```haskell 
sayHello3 :: Action World ()
sayHello3 = Action (\w ->
    let (w', n) = (runAction nameFromArgs3) w

    in (runAction (putStrLn $ "Hello " ++ n)) w')

nameFromArgs3 :: Action World String
nameFromArgs3 = Action (\w ->
    let (w', args) = (runAction getArgs) w

    in case args of
        ("-u":name:_) -> (w', map toUpper name)
        (     name:_) -> (w', name)
        otherwise     -> (w', "who?"))
```

This allows us to use `sayHello3` directly in `main`:

```haskell 
main4 :: [String] -> String
main4 args = output $ execAction sayHello3 (World args "")
```

Things are still pretty clunky, but one thing to notice is that now all 
of the world-changing things are of the same type, specifically `Action 
World a`. Getting things to all be the same type has exposed the 
underlying duplication involved with sequencing lists of actions over 
some world.

## A Monad is Born

One obvious duplication is taking two `Action`s and combining them into 
one `Action` which represents passing a `World` through them, one after 
another.

```haskell 
combine :: Action w a -> Action w b -> Action w b
combine f g = Action (\w ->
    -- call the first action on the world given to produce a new world,
    let (w',  _) = (runAction f) w

        -- then call the second action on that new world
        (w'', b) = (runAction g) w'

    -- to produce the final world and result
    in (w'', b))

f = combine (putStrLn "one") (putStrLn "two")

execAction f $ World [] ""
-- => World [] "one\ntwo\n"
```

What about functions like `putStrLn` which aren't themselves an `Action` 
until they've been given their first argument? How can we combine those 
with other `Action`s?

```haskell 
pipe :: Action w a -> (a -> Action w b) -> Action w b
pipe f g = Action (\w ->
    -- call the first action on the world given to produce a new world 
    -- and a result of type a,
    let (w',  a) = (runAction f) w

        -- then give the result of type a to the second function which 
        -- turns it into an action which can be called on the new world
        (w'', b) = (runAction (g a)) w'

    -- to produce the final world and result
    in (w'', b))

f = pipe getArgs (putStrLn . head)

execAction f $ World ["Pat"] ""
-- => World ["Pat"] "Pat\n"
```

`pipe` and `combine` both require their first argument be an `Action`, 
but what if all we have is a non-`Action` value?

```haskell 
-- turn the value into an Action by returning it as the result along 
-- with the world given
promote :: a -> Action w a
promote x = Action (\w -> (w, x))

f = pipe (promote "Hello world") putStrLn

execAction f $ World [] ""
-- => World [] "Hello world\n"
```

Finally, we can remove that duplication and make our code much more 
readable:

```haskell 
sayHello4 :: Action World ()
sayHello4 = pipe nameFromArgs4 (\n -> putStrLn $ "Hello " ++ n)

nameFromArgs4 :: Action World String
nameFromArgs4 =
    pipe getArgs (\args ->
        promote $ case args of
                    ("-u":name:_) -> map toUpper name
                    (     name:_) -> name
                    otherwise     -> "who?")
```

Turns out, the behaviors we've just defined have a name: Monad. And once 
you've made your type a Monad (by defining these three functions), any 
and all functions which have been written to deal with Monads (which is 
a lot) will now be able to work with your type.

To show that there are no tricks here, I'll even use the functions we've 
defined as the implementation in our real Monad instance:

```haskell 
instance Monad (Action w) where
    return = promote
    (>>=)  = pipe

-- As our first free lunch, Haskell already provides "combine" in terms 
-- of >>=. A combination is just a pipe but with the result of the first 
-- action discarded.
(>>) f g = f >>= \_ -> g
```

Now our functions are looking like real Haskell syntax:

```haskell 
sayHello5 :: Action World ()
sayHello5 = nameFromArgs5 >>= (\n -> putStrLn $ "Hello " ++ n)

nameFromArgs5 :: Action World String
nameFromArgs5 =
    getArgs >>= \args ->
        return $ case args of
                    ("-u":name:_) -> map toUpper name
                    (     name:_) -> name
                    otherwise     -> "who?"
```

## Do It to It

Now that we've made our type a real Monad, and now that we understand 
what functions like `return` and `(>>=)` mean, we can make the final 
leap to the more imperative looking code we started with.

Haskell has something called "do-notation". All it is is a form of 
pre-processing which transforms expressions like this:

```haskell 
f = do
  args <- getArgs

  putStrLn $ head args
```

Into expressions like this:

```haskell 
f = getArgs >>= (\args -> putStrLn $ head args)
```

Either syntax is valid Haskell, and I use both freely depending on the 
scenario. Let's go ahead and rewrite our functions in do-notation:

```haskell 
sayHello6 :: Action World ()
sayHello6 = do
    name <- nameFromArgs5

    putStrLn $ "Hello " ++ name

nameFromArgs6 :: Action World String
nameFromArgs6 = do
    args <- getArgs

    return $ case args of
                ("-u":name:_) -> map toUpper name
                (     name:_) -> name
                otherwise     -> "who?"
```

It's hard to believe that, to this point, we have no such thing as `IO`. 
These functions simply describe how to make one `World` from another, 
and that only actually happens when `main` puts `sayHello` together with 
some initial `World` via `execAction`.

What we've done is built the system we want for IO all the way up to 
`main`. We've given any function in our system "direct" access to 
program input and output, all that's required is they make themselves 
`Action`s. Through the use of the `Monad` typeclass and do-notation, 
making functions `Action`s has become quite pleasant while keeping 
everything entirely pure.

## Final Touches

Let's say that instead of being a primitive `[String] -> String`, we'll 
let `main` be itself an `Action World ()`. Then we can let the Haskell 
runtime handle constructing a `World`, calling `execAction main` on it, 
then outputting whatever `output` there is in the new `World` we get 
back.

Then, let's imagine we didn't have our simplistic `World` type which 
only deals with commandline arguments and an output string. Imagine we 
had a rich `World` that knew about environment variables, file handles, 
and memory locations. That type would live in an impure space with 
access to all the richness of reality, but we could use pure `Action`s 
to describe how to read its files or access its volatile memory.

Things might end up like this:

```haskell 
type IO a = Action World a

main :: IO ()
main = do
    args <- getArgs

    let name = case args of
                ("-u":n:_) -> map toUpper n
                (     n:_) -> n
                otherwise  -> "who?"

    putStrLn $ "Hello " ++ name
```

```
$ runhaskell hello.hs -u io
Hello IO
```
