---
layout: post
title: "Maybe Is Just Awesome"
tags:
  - haskell
---

In Haskell, functions must always return the same consistent type. There 
is also no concept of `nil` or `null` built into the language. This is 
not meant to handicap you, and the expressiveness and polymorphic-ness 
of Haskell's types mean it certainly does not. One way to handle such 
situations where functions (conceptually) may or may not return a value 
is through the `Maybe` type.

```haskell 
data Maybe a = Just a | Nothing
```

`Maybe` is a perfect and simple solution for this situation. It says 
that, for all values of type `a`, we can construct values that are 
either `Just` that or `Nothing`.

This type is also perfect for illustrating some of Haskell's more 
math-heavy concepts. If we take all the potential `a` values as one 
category and all the potential `Maybe a` values as another, then we can 
use this type to describe the `Functor`s of Category Theory. If we also 
think about the difference between some `a` and its `Maybe a` 
counterpart as some sort of state to be managed throughout an execution 
chain, then we can also use this type to describe a `Monad`. In both 
cases, the benefits are more concise code and a greater understanding of 
these abstract concepts that we can take with us to more complex type 
interactions.

## Functor

A `Functor` is a way to transform a function (more formally a 
*morphism*) that acts on one category of objects into one that can act 
on objects in another category. In Haskell, this concept is captured by 
the `Functor` typeclass. It states that for any type `t` that takes one 
argument (like `Maybe`), we can make it an instance of `Functor` by 
defining the translation function `fmap`:

```haskell 
fmap :: Functor t => (a -> b) -> (t a -> t b)
```

This specifies precisely how a function that acts on one set of types 
(`a -> b`) can be used on types that are wrapped versions of these (`t a 
-> t b`).

So how is `Maybe` a `Functor`?

```haskell 
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just a) = Just (f a)
```

Seems straight forward, if the value is `Just` we apply the morphism to 
the underlying object and rewrap the result in `Just`. Trying to apply a 
morphism to a `Nothing` value just results in `Nothing`.

## Monad

A `Monad` is a very scary term to Haskell noobies. Mainly because the 
first `Monad` we are introduced to is `IO`. It's used for any 
computation that affects (or draws on) the outside world. We're told 
that it handles potential failures and manages state between 
computations. Most times we accept it as magic and blindly memorize the 
`do` notation and counter-intuitive `return` statements.

We can take a step back, talk about a `Monad` in very general terms, 
then describe how `Maybe` types work as a `Monad`. Given that 
understanding, we can get a better handle on what `State` and `IO` 
`Monad`s are doing (even if we still have to think of it as a bit of 
magic).

A `Monad` is a way to chain multiple computations together and manage 
how that chain of actions works as a whole. The `Monad` laws will manage 
the state between these actions (ensuring dependent actions are run in 
the correct order since they might rely on more than just their direct 
arguments) and also any failing cases (if some action fails, future 
actions are aborted and the whole expression is a failure).

Somewhat surprisingly, any type can act as a `Monad` by defining a few 
simple functions. I'm going to show and talk about them separately 
because I think it can go a long way to understanding `Monad`s in 
general.

```haskell 
instance Monad Maybe where
    -- (>>=) :: m a -> (a -> m b) -> m b
    (Just x) >>= k = k x
    Nothing  >>= _ = Nothing
```

Here we're just showing how to chain two dependant actions together -- 
that's really all it is. The first "action" is a wrapped value (`m a`), 
the second argument is a function which acts on the unwrapped value 
producing a new wrapped value (`a -> m b`). For `Maybe` we just have to 
account for the `Just` and `Nothing` cases appropriately.

```haskell 
    -- (>>) :: m a -> m b -> m b
    (Just _) >> k = k
    Nothing  >> _ = Nothing
```

Here we're showing how to chain two *independant* actions together. 
We're still preserving the fact that if the first action "fails" the 
second action is not run, but in this case the result of the first 
action has no bearing on the second.

```haskell 
    -- return :: a -> m a
    return = Just
```

`return` is simply a way to take some non-monadic value and treat it as 
a `Monadic` action. In our case wrapping a value in `Just` does just 
that.

```haskell 
    -- fail :: String -> m a
    fail _ = Nothing
```

There's also the concept of outright failure. For us it's simple: 
`Nothing` is the failure case. The reason for the `String` argument is 
that Haskell allows you to include a message with the failure. There's 
much contention in the Haskell community around including `fail` in the 
`Monad` type class, but we won't get into that here as `Maybe` has a 
pretty simple implementation of it.

It should also be noted that the `do` and `<-` notation that everyone is 
used to can be "de-sugared" down to an expression using only the above 4 
functions. If you're having trouble seeing how an expression is 
leveraging the above laws to do what it does, it can be a good exercise 
to de-sugar it by hand.

<div class="note">
The super interesting thing (I find) about the above instances of 
`Functor` and `Maybe` is that we're not making `Maybe a` an instance of 
anything, we're describing *only* the behavior of `Maybe`. The types 
being wrapped up are irrelevant (they can even be further wrapped in 
`Maybe` or `IO` -- crazy).

Leaving those details out of it, or more importantly *being able* to 
leave those details out of it is just another case of Haskell's type 
system leading to elegant and generalized code.

</div>

## Example Time

So why do we care? Well, besides using `Maybe` as an illustration for 
hard-to-grasp concepts like `Functor`s and `Monad`s, knowing when to 
use those instances of `Maybe` can really cut down on code clutter and 
lead to elegant solutions when you've got a lot of `Maybe`-heavy code.

Let's say you've got a user model in your webapp with an optional email 
field. This field is a custom type `Email` but you've got a function for 
translating it to `Text`. You've also got another general function for 
displaying `Text` values on the page as `Html`.

Because you were thinking ahead and you knew there'd be a lot of `Maybe 
Text` values in use throughout your site, you've coded your `display` 
function to accept maybe values and show an empty string in these cases.

```haskell 
userEmail :: User -> Maybe Email
userEmail = undefined

emailToText :: Email -> Text
emailToText = undefined

display :: Maybe Text -> Html
display = undefined
```

In the described ecosystem your going to have a lot of core `Maybe` 
values and a lot of value-manipulation functions not in `Maybe`. To put 
this in category terms, you've got a lot of morphisms in the non-maybe 
category and a lot of objects in the maybe category. You're going to 
want to `fmap` that.

Here's how the code looks without leveraging the fact that `Maybe` is a 
`Functor`:

```haskell 
displayUserEmail :: User -> Html
displayUserEmail u = let me = userEmail u
                     in display $ case me of
                         Just e  -> Just (emailToText e))
                         Nothing -> Nothing
```

Not terrible, but notice how `fmap` shrinks it right up:

```haskell 
displayUserEmail :: User -> Html
displayUserEmail = display . fmap emailToText . userEmail
```

Not only does it make the code clearer and cleaner, but it serves a 
common purpose: you're going to have a lot of value-manipulating 
functions that should operate on basic values and not care about any 
wrapping. Just because you've got a lot of these values wrapped up in 
`Maybe`, that shouldn't stop you from using these *morphisms from the 
other category* in this one. The nature of that `Maybe` wrapper allows 
`fmap` to easily handle the translation for you.

Sure, you could write a small function that takes functions that operate 
on normal values and allows them to be used on maybe values (and I think 
I did just that at one point) -- but this concept of a `Functor` 
abstracts all that down to a simple generic `fmap` that can be used with 
a zillion different compound "wrapper" types.

<div class="note">
Guess what? `IO` is a `Functor` too.

```haskell 
-- something like this:
prettyNow :: IO String
prettyNow = do
  now <- getCurrentTime

  return $ formatPretty now

-- can be shorter:
prettyNow = fmap formatPretty getCurrentTime
```

Oh, and if you're interested in seeing how `do` notation is de-sugared, 
here's that first, non-functor version but without the `do` notation:

```haskell 
prettyNow :: IO ()
prettyNow =
    getCurrentTime >>= \now ->
        return (formatPretty now)
```

Coming back to our Monadic laws, you can imagine that if 
`getCurrentTime` failed in some way (and we know `IO` has *some* 
implementation for `fail`) then the entire expression will be `fail` 
simply because of the mechanics behind `>>=`.

</div>

Using `Maybe` as a `Monad` allows for even more verbose "stair-case" 
code to become much more readable. For this example, we've got a series 
of functions that translate values from one type to another. Any of 
these functions can fail if the input is not as expected and they 
capture this by returning maybe values:

```haskell 
textToXml :: Text -> Maybe Xml
textToXml = undefined

xmlToJson :: Xml -> Maybe Json
xmlToJson = undefined

jsonToResponse :: Json -> Maybe Response
jsonToResponse = undefined
```

As before, here's that code written in a way that does not leverage 
`Maybe`'s monadic properties:

```haskell 
textToResponse :: Text -> Maybe Response
textToResponse t = let mx = textToXml t
                   in case mx of
                       Nothing -> Nothing
                       Just x  -> let mj = xmlToJson x
                                  in case mj of
                                      Nothing -> Nothing
                                      Just j  -> jsonToRepsonse j
```

What do you have here? A series of dependant computations where if any 
one of them fails we want the whole expression to fail. Strictly using 
what we've learned in this post, we can simplify this to the following:

```haskell 
textToResponse :: Text -> Maybe Response
textToResponse t = textToXml t >>= xmlToJson >>= jsonToResponse
```

And if you prefer `do` notation (I do), then we could write the above 
like so:

```haskell 
textToResponse :: Text -> Maybe Response
textToResponse t = do
    x <- textToXml t
    j <- xmlToJson x
    r <- jsonToResponse j

    return r
```

The `r <-` and `return r` is redundant but I think it shows more clearly 
the interaction between the `a`s and `Maybe a`s.

<div class="note">
You can even mix `do` notations within each other:

```haskell 
main :: IO ()
main = do
    -- this is IO
    text <- getSomeText
  
    let mresponse = do
            -- but this is Maybe
            x <- textToXml t
            j <- xmlToJson x
            r <- jsonToResponse j
  
            return r
  
    -- and IO again
    sendResponse mresponse
```

</div>

So hopefully you've all learned a little bit through this post. I know 
it was helpful for me to write it all out. We've seen that `Maybe` is a 
type that is complex enough to be used in a variety of different 
contexts but also simple enough to illustrate those contexts in an 
easier to grasp way. We've also seen that using these higher-level 
qualities of `Maybe` can lead to smaller, easier to read code.
