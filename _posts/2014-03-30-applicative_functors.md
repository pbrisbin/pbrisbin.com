---
layout: post
title: "Applicative Functors"
tags:
  - haskell
  - applicative
---

Every time I read [Learn You a Haskell][lyah], I get something new out of it.
This most recent time through, I think I've finally gained some insight into the
`Applicative` type class.

[lyah]: http://learnyouahaskell.com

I've been writing Haskell for some time and have developed an intuition and
explanation for `Monad`. This is probably because monads are so prevalent in
Haskell code that you can't help but get used to them. I knew that `Applicative`
was similar but weaker, and that it should be a super class of `Monad` but since
it arrived later it is not. I now think I have a general understanding of how
`Applicative` is different, why it's useful, and I would like to bring anyone
else who glossed over `Applicative` on the way to `Monad` up to speed.

The `Applicative` type class represents applicative functors, so it makes sense
to start with a brief description of functors that are *not* applicative.

## Values in a Box

A functor is any container-like type which offers a way to transform a normal
function into one that operates on contained values.

Formally:

```haskell
fmap :: Functor f    -- for any functor,
     => (  a ->   b) -- take a normal function,
     -> (f a -> f b) -- and make one that works on contained values
```

Some prefer to think of it like this:

```haskell
fmap :: Functor f -- for any functor,
     => (a -> b)  -- take a normal function,
     -> f a       -- and a contained value,
     -> f b       -- and return the contained result of applying that 
                  -- function to that value
```

Because Haskell functions are [curried][currying] by default, we can reason
about and use this function either way -- with the former being more useful to
the current discussion.

[currying]: http://www.haskell.org/haskellwiki/Currying

This is the first small step in the ultimate goal between all three of these
type classes: allow us to work with values with context (in this case, a
container of some sort) as if that context weren't present at all. We give a
normal function to `fmap` and it sorts out how to deal with the container,
whatever it may be.

## Functions in a Box

To say that a functor is "applicative", we mean that the contained value *can be
applied*. In other words, it's a function.

An applicative functor is any container-like type which offers a way to
transform a *contained* function into one that can operate on contained values.

```haskell
(<*>) :: Applicative f -- for any applicative functor,
      => f (a ->   b)  -- take a contained function,
      -> (f a -> f b)  -- and make one that works on contained values
```

Again, we could also think of it like this:

```haskell
(<*>) :: Applicative f -- for any applicative functor,
      => f (a -> b)    -- take a contained function,
      -> f a           -- and a contained value,
      -> f b           -- and return a contained result
```

Applicative functors also have a way to take an un-contained function and put it
into a container:

```haskell
pure :: Applicative f -- for any applicative functor,
     =>   (a -> b)    -- take a normal function,
     -> f (a -> b)    -- and put it in a container
```

In actuality, the type signature is simpler: `a -> f a`. Since `a` literally
means "any type", it can certainly represent the type `(a -> b)` too.

```haskell
pure :: Applicative f => a -> f a
```

Understanding this is very important for understanding the usefulness of
`Applicative`. Even though the type signature for `(<*>)` starts with `f (a ->
b)`, it can also be used with functions taking any number of arguments.

Consider the following:

```haskell
:: f (a -> b -> c) -> f a -> f (b -> c)
```

Is this `(<*>)` or not?

Instead of writing its signature with `b`, lets use a question mark:

```haskell
(<*>) :: f (a -> ?) -> f a -> f ?
```

Indeed it is: Substitute the type `(b -> c)` for every `?`, rather than the
simple `b` in the actual class definition.

## Curried All the Way Down

What you just saw was a very concrete example of the benefits of currying by
default. When we say "a function of *n* arguments", we're actually lying. All
functions in Haskell take exactly one argument. Multi-argument functions are
really single-argument functions that return other single-argument functions
that accept the remaining arguments via the same process.

Using the question mark approach, we see that multi-argument functions are
actually of the form:

```haskell
f :: a -> ?
f = -- ...
```

And it's entirely legal for that `?` to be replaced with `(b -> ?)`, and for
*that* `?` to be replaced with `(c -> ?)` and so on ad infinitum. Thus you have
*the appearance* of multi-argument functions.

As is common with Haskell, this results in what appears to be happy coincidence,
but is actually the product of developing a language on top of such a consistent
mathematical foundation. You'll notice that after using `(<*>)` on a function of
more than one argument, the result is not a wrapped result, but another wrapped
function -- does that sound familiar? Exactly, it's an applicative functor.

Let me say that again: if you supply a function of more than one argument and a
single wrapped value to `(<*>)`, you end up with another applicative functor
which can be given to `(<*>)` yet again with another wrapped value to supply the
remaining argument to that original function. This can continue as long as the
function needs more arguments. Exactly like normal function application.

## A "Concrete" Example

Consider what this might look like if you start with a plain old function that
(conceptually) takes more than one argument, but the values that it wants to
operate on are wrapped in some container.

```haskell
-- A normal function
f :: (a -> b -> c)
f = -- ...

-- One contained value, suitable for its first argument
x :: Applicative f => f a
x = -- ...

-- Another contained value, suitable for its second
y :: Applicative f => f b
y = -- ...
```

How do we pass `x` and `y` to `f` to get some overall result? You wrap the
function with `pure` then use `(<*>)` repeatedly:

```haskell
result :: Applicative f => f c
result = pure f <*> x <*> y
```

The first portion of that expression is very interesting: `pure f <*> x`. What
is this bit doing? It's taking a normal function and applying it to a contained
value. Wait a second, normal functors know how to do that!

Since in Haskell every `Applicative` is also a `Functor`, that means it could be
rewritten equivalently as `fmap f x`, turning the whole expression into `fmap f
x <*> y`.

Never satisfied, Haskell introduced a function called `(<$>)` which is just
`fmap` but infix. With this alias, we can write:

```haskell
result = f <$> x <*> y
```

Not only is this epically concise, but it looks exactly like `f x y` which is
how this code would be written if there were no containers involved. Here we
have another, more powerful step towards the goal of writing code that has to
deal with some context (in our case, still that container) without actually
having to care about that context. You write your function like you normally
would, then add `(<$>)` and `(<*>)` between the arguments.

## Container &rArr; Context

For the purpose of `Monad`, we should avoid the word *container* and use only
the word *context*. The `f` in `Functor f => f a` or the `m` in `Monad m => m a`
is a *context* surrounding a value of type `a`. It represents some bit of other
information. In the case of `Maybe`, it represents the fact that the value may
or may not be there. In the case of lists, it represents that the value may be
one of many possibilities. In the case of `IO`, it represents that some
interaction with the outside world must occur to produce the value.

The power added by `Monad` is the ability to alter that context as part of the
composition of values. This power comes from its `join` operation, though the
way the `Monad` type class was originally written in Haskell makes that a bit
convoluted to discover.

## Bind

In the actual `Monad` type class, we have `(>>=)` (pronounced *bind*). Rather
that starting here though, I'll start with its flipped variant: the same
function, only taking its arguments in reverse order:

```haskell
(=<<) :: Monad m    -- for any monad,
      -> (a -> m b) -- take a function from a value of type a to a value of type
                    -- b, but with some additional context,
      -> m a        -- and a value of type a with some context,
      -> m b        -- and return a value of of type b with some context
```

The reason I chose to start here is that, when we add some explicit parenthesis,
we can see how the three type classes line up:

```haskell
-- Functor: take a pure function from a to b, and return a function suitable for
-- values with context:
(<$>) :: (a -> b) -> (f a -> f b)

-- Applicative: take a pure function from a to b, which has its own context, and
-- return a function suitable for values with context:
(<*>) :: f (a -> b) -> (f a -> f b)

-- Monad: take a function from a to "b with context", and return a function
-- suitable for values with context:
(=<<) :: (a -> f b) -> (f a -> f b)
```

*Note: in much of this section, I'll use `f` to represent the Functor,
Applicative, or Monad (which is usually `m`) in type annotations so you can see
how similar they really are.*

We can now see that the difference between `Applicative` and `Monad` is that one
can only work with `f (a -> b)` but the other can work with `a -> f b`. What
does this mean?

Well, since our input function is now producing its own context, it gives us to
ability to alter the context which is ultimately returned. In order for us
*producing* a context to have the effect of *altering* the result's context,
there needs to be some facility by which `(=<<)` is resolving what we produce
and what we're given into a single result. That's `join`.

## Join

The `join` function has the following type:

```haskell
join :: m (m a) -> m a
```

It describes a way to *flatten* contexts. By applying `join`, the outer `m` and
the inner `m` are resolved to a single value. This is how the context we produce
and the context we're given get resolved by `(=<<)`.

For historical reasons, all monadic functions (including `join`) are implemented
in terms of `(>>=)` and `return`, but its truer (in the Category Theory sense)
to let `join` and `return` be the foundational functions, assume any `Monad` is
also a `Functor` and `Applicative`, and implement all other functions in terms
of them:

```haskell
-- Relying on join + Functor
f (=<<) ma = join $ fmap f ma

-- Or relying on join + Applicative
f (=<<) ma = join $ pure f <*> ma
```

## What's the Point?

With all of this background knowledge, I came to a simple mental model for
applicative functors vs monads: *Monad is for series where Applicative is for
parallel*.

With a `join` function in hand, we have the ability to resolve multiple
contexts. This means that we can produce and resolve contexts along the way to
modify how the expression evolves in reaction to the (contextual) values we see.
This allows us to model things like *do this then that* or *do this when that*.
With `Applicative`, we don't have the power to setup dependencies like that by
inspecting and modifying contexts along the way. We can only model things like
*do this and that*.

Let's walk through a real example.

## Building a User

In an application I'm working on, I'm doing OAuth based authentication. My
domain has the following (simplified) user type:

```haskell
data User = User
    { userFirstName :: Text
    , userLastName  :: Text
    , userEmail     :: Text
    }
```

During the process of authentication, an OAuth endpoint provides me with some
profile data which ultimately comes back as an association list:

```haskell
type Profile = [(Text, Text)]

-- Example:
-- [ ("first_name", "Pat"            )
-- , ("last_name" , "Brisbin"        )
-- , ("email"     , "me@pbrisbin.com")
-- ]
```

Within this list, I can find user data via the `lookup` function which takes a
key and returns a `Maybe` value. I had to write the function that builds a
`User` out of this list of profile values. I also had to propagate any `Maybe`
values by returning `Maybe User`.

First, let's write this without exploiting the fact that `Maybe` is a monad or
an applicative:

```haskell
buildUser :: Profile -> Maybe User
buildUser p =
    case lookup "first_name" p of
        Nothing -> Nothing
        Just fn -> case lookup "last_name" p of
            Nothing -> Nothing
            Just ln -> case lookup "email" p of
                Nothing -> Nothing
                Just e  -> Just $ User fn ln e
```

Oof.

Treating `Maybe` as a `Monad` makes this much, much cleaner:

```haskell
buildUser :: Profile -> Maybe User
buildUser p = do
    fn <- lookup "first_name" p
    ln <- lookup "last_name" p
    e  <- lookup "email" p

    return $ User fn ln e
```

Up until a few weeks ago, I would've stopped there and been extremely proud of
myself and Haskell. Haskell for supplying such a great abstraction for potential
failed lookups, and myself for knowing how to use it.

Hopefully, the content of this blog post has made it clear that we can do
better.

## Series vs Parallel

Think about the thing we're modelling here. We're not using the power that comes
with `Monad` in any way. Nowhere do we inspect a single value and alter the flow
of control through the expression. If one key's not found, we want `Nothing`
regardless of which key it is or when it goes missing. What we're really doing
here is taking the three values with context (the `Maybe` profile values) and
combining them all together via the `User` data constructor.

This is `Applicative`, [I know this][unix].

[unix]: https://www.youtube.com/watch?v=dFUlAQZB9Ng

```haskell
-- f :: a    -> b    -> c    -> d
User :: Text -> Text -> Text -> User

-- x                  :: f     a
lookup "first_name" p :: Maybe Text

-- y                 :: f     b
lookup "last_name" p :: Maybe Text

-- z             :: f     c
lookup "email" p :: Maybe Text

-- result :: f d
-- result = f <$> x <*> y <*> z
buildUser :: Profile -> Maybe User
buildUser p = User
    <$> lookup "first_name" p
    <*> lookup "last_name" p
    <*> lookup "email" p
```

And now, I understand when to reach for `Applicative` over `Monad`. Perhaps you
do too?
