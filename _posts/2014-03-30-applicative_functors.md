---
layout: post
title: "Applicative Functors"
tags:
  - haskell
  - applicative
---

Every time I read [Learn You a Haskell][lyah], I get something new out 
of it. This most recent time through, I think I've finally gained some 
insight into the `Applicative` type class.

[lyah]: http://learnyouahaskell.com

I've been writing Haskell for some time and have developed an intuition 
and explanation for `Monad`. This is probably because monads are so 
prevalent in Haskell code that you can't help but get used to them. I 
knew that `Applicative` was similar but weaker, and that it should be a 
super class of `Monad` but since it arrived later it is not. I now think 
I have a general understanding of how `Applicative` is different, why 
it's useful, and I would like to bring anyone else who glossed over 
`Applicative` on the way to `Monad` up to speed.

The `Applicative` type class represents applicative functors, so it 
makes sense to start with a brief description of functors that are *not* 
applicative.

## Values in a Box

A functor is any container-like type which offers a way to transform a 
normal function into one that operates on contained values.

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

Thanks to currying, the two are completely equivalent.

This is the first small step in the ultimate goal between all three of 
these type classes: allow us to work with values with context (in this 
case, a container of some sort) as if that context weren't present at 
all. We give a normal function to `fmap` and it sorts out how to deal 
with the container, whatever it may be.

## Functions in a Box

To say that a functor is "applicative", we mean that the contained value 
*can be applied*. This is just another way of saying it's a function.

An applicative functor is any container-like type which offers a way to 
transform a *contained* function into one that can operate on contained 
values.

```haskell
(<*>) :: Applicative f -- for any applicative functor,
      => f (a ->   b)  -- take a contained function,
      -> (f a -> f b)  -- and make one that works on contained values
```

Again because of currying, we can also think of it like this:

```haskell
(<*>) :: Applicative f -- for any applicative functor,
      => f (a -> b)    -- take a contained function,
      -> f a           -- and a contained value,
      -> f b           -- and return a contained result
```

Applicative functors also have a way to take an un-contained function 
and put it into a container:

```haskell
pure :: Applicative f -- for any applicative functor,
     =>   (a -> b)    -- take a normal function,
     -> f (a -> b)    -- and put it in a container
```

In actuality, the type signature is just `a -> f a`. Since `a` literally 
means "any type", it can certainly represent the type `(a -> b)` too.

```haskell
pure :: Applicative f => a -> f a
```

Understanding this is very important for understanding the usefulness of 
`Applicative`. Even though the type signature for `(<*>)` starts with `f 
(a -> b)`, it can just as easily be used with functions taking any 
number of arguments.

Consider the following:

```haskell
:: f (a -> b -> c) -> f a -> f (b -> c)
```

Is this `(<*>)` or not?

Instead of writing its signature with `b`, lets use a question mark:

```haskell
(<*>) :: f (a -> ?) -> f a -> f ?
```

Indeed it is. Just substitute the type `(b -> c)` for every `?` rather 
than the simple `b` in the actual class definition.

## Curried All the Way Down

What you just saw was a very concrete example of currying. When we say 
"a function of *n* arguments", we're actually lying. All functions in 
Haskell take exactly one argument. Multi-argument functions are really 
just single-argument functions that return other single-argument 
functions that accept the remaining arguments via the same process.

Using the question mark approach, we see that multi-argument functions 
are simply the form:

```haskell
f :: a -> ?
f = -- ...
```

And it's entirely legal for that `?` to be replaced with `(b -> ?)`, and 
for *that* `?` to be replaced with `(c -> ?)` and so on ad infinitum. 
Thus you have *the appearance* of multi-argument functions.

As is common with Haskell, this results in what appears to be happy 
coincidence, but is actually the product of developing a language on top 
of such a consistent mathematical foundation. You'll notice that after 
using `(<*>)` on a function of more than one argument, the result is not 
a wrapped result, but another wrapped function -- does that sound 
familiar? Exactly, it's an applicative functor.

Let me say that again: if you partially apply a function of more than 
one argument using `(<*>)`, you end up with another applicative functor 
which can be given to `(<*>)` yet again with another wrapped value to 
supply the remaining argument to that original function. This can 
continue as long as the function needs more arguments. Just like normal 
function application.

## A "Concrete" Example

Consider what this might look like if you start with a plain old 
function that (conceptually) takes more than one argument, but the 
values that it wants to operate on are wrapped in some container.

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

How do we pass `x` and `y` to `f` to get some overall result? Easy, you 
wrap the function with `pure` then use `(<*>)` repeatedly:

```haskell
result :: Applicative f => f c
result = pure f <*> x <*> y
```

The first portion of that expression is very interesting: `pure f <*> 
x`. What is this bit doing? It's taking a normal function and applying 
it to a contained value. Wait a second, normal functors know how to do 
that!

Since in Haskell every `Applicative` is also a `Functor`, that means it 
could be rewritten as just `fmap f x`, turning the whole expression into 
`fmap f x <*> y`.

Never satisfied, Haskell introduced a function called `(<$>)` which is 
just `fmap` but infix. With this alias, we can write:

```haskell
result = f <$> x <*> y
```

Not only is this epically concise, but it looks exactly like `f x y` 
which is how this code would be written if there were no containers 
involved. Here we have another, more powerful step towards the goal of 
writing code that has to deal with some context (in our case, still that 
container) without actually having to care about that context. You write 
your function like you normally would, then just pepper `(<$>)` and 
`(<*>)` between the arguments.

## A Missing Piece

With both `Functor` and `Applicative`, anything and everything was 
wrapped. Both arguments to `(<*>)` are wrapped, the result is wrapped, 
and `pure` wraps something up. We never have to deal with *unwrapping* 
anything.

Simply put, a `Monad` is a type that can do everything an `Applicative` 
can do plus handle unwrapping. However, it can't just unwrap values 
willy-nilly. It can only unwrap a value in a very specific case: while 
passing it to a function which returns a wrapped result.

Formally:

```haskell
(>>=) :: Monad m    -- for any monad,
      => m a        -- take wrapped value
      -> (a -> m b) -- and a function which needs it unwrapped
      -> m b        -- and return something the same type as that function
                    -- returns
```

This clarifies why it's the only way we can support unwrapping. We're 
taking a wrapped value and producing *a function which operates on an 
unwrapped value*. The type signature describes the nature of this 
function: it takes yet another wrapped value as argument and produces a 
wrapped value of the same type as its result.

This gives us the needed flexibility to implement unwrapping. Consider a 
type like `Maybe`. If we were able to unwrap values at any point and 
return them directly, we'd be in trouble when we come across a 
`Nothing`. If, on the other hand, our type signature says we ourselves 
have to return a wrapped result, we can take the reasonable step of not 
unwrapping anything and simply returning another `Nothing`.

The above type signature ensures that's **always** an option.

Haskell has no generic function of the type `Monad m => m a -> a`. 
Without that, there is no opportunity for unwrapping something that 
can't be unwrapped. Haskell does have a function called `join` with the 
signature `Monad m => m (m a) => m a`. This is indeed a function that 
just unwraps a value directly, but because the type signature enforces 
that the value coming in is doubly-wrapped and the value going out is 
still wrapped, we can maintain our safety. Yay type systems.

## Wrapper &rArr; Action, Unwrapping &rArr; Sequencing

Up until now, we've been calling these types wrappers, containers, or 
contexts. With `Monad` it can be easier to think of them as *actions*. 
An action implies that *something else* may occur as a result of 
evaluating this otherwise pure function: side-effects. These can be 
real-world side effects in the case of `IO`, or context-changing side 
effects in the case of `Maybe` or `List`.

Unwrapping as a concept should then be replaced with *evaluating* or 
*running* an action, it's when any side-effects will be realized. Again 
in the case of `Maybe`, when we attempt to unwrap a `Nothing` value via 
`(>>=)`, that's the point at which the entire computation becomes a 
`Nothing`.

Once we've made that conceptual leap, we can think about *dependent*, or 
*sequenced* actions. In the case of `IO`, we have an expectation that 
actions will be performed in a particular order. In the case of `Maybe`, 
we need to know that if an *earlier* function returns `Nothing`, the 
*later* functions will know about it.

The ability for a `Monad` to be unwrapped or evaluated combined with the 
type signature of `(>>=)` provides for sequencing because it enforces 
that the left hand side is evaluated before the right hand side. This 
must be true because the left hand value has to be evaluated (i.e. 
unwrapped) for the right hand side to even be evaluable at all.

## What's the Point?

With all of this background knowledge, I came to a simple mental model 
for applicative functors vs monads: *Monad is for series where 
Applicative is for parallel*.

We use a monad for composing multiple actions (values with context) into 
a single action (a new value with context). We use applicative for the 
same reason. The difference lies (of course) in how that composition is 
carried out. With a monad, each action is evaluated in turn and the 
results of each are fed into the next via `(>>=)`. This implies 
ordering. With an applicative functor, every value is unwrapped in turn 
as functions are applied via `(<*>)` and the results are combined into a 
single value in "parallel".

Let's walk through a real example.

## Building a User

In an application I'm working on, I'm doing OAuth based authentication. 
My domain has the following (simplified) user type:

```haskell
data User = User
    { userFirstName :: Text
    , userLastName  :: Text
    , userEmail     :: Text
    }
```

During the process of authentication, an OAuth endpoint provides me with 
some profile data which ultimately comes back as an association list:

```haskell
type Profile = [(Text, Text)]

-- Example:
-- [ ("first_name", "Pat"            )
-- , ("last_name" , "Brisbin"        )
-- , ("email"     , "me@pbrisbin.com")
-- ]
```

Within this list, I can find user data via the `lookup` function which 
takes a key and returns a `Maybe` value. I had to write the function 
that builds a `User` out of this list of profile values. I also had to 
propagate any `Maybe` values by returning `Maybe User`.

First, let's write this without exploiting the fact that `Maybe` is a 
monad or an applicative:

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

Up until a few weeks ago, I would've stopped there and been extremely 
proud of myself and Haskell. Haskell for supplying such a great 
abstraction for potential failed lookups, and myself for knowing how to 
use it.

Hopefully, the content of this blog post has made it clear that we can 
do better.

## Series vs Parallel

Think about the thing we're modelling here. A monad is best used for 
sequencing dependant actions with side-effects. Does it matter in what 
order we look things up? If one key's not found, we want `Nothing` 
regardless of which key it is or when it goes missing. What we're really 
doing here is taking the three values with context (the `Maybe` profile 
values) and combining them all together via the `User` data constructor.

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

And now, I understand when to reach for `Applicative` over `Monad`. 
Perhaps you do too?
