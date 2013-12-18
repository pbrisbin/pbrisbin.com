---
layout: post
title: "Monoids"
tags:
  - haskell
  - monoid
---

Functional programming in general, and Haskell in particular seems to be 
gaining popularity lately. If not with actual increased usage, at least 
with increased interest. As a resident Haskell-fanboy at my office, I'm 
often asked why I enjoy the language so much. The difficulty in 
answering such a question is that the parts of the language I enjoy the 
most are the more advanced and interesting concepts (like Monads and 
Functors) which are extremely difficult to explain over drinks at a bar 
-- and believe me, I've tried.

The fact that you can build and use an abstraction like Monad so 
effortlessly is the reason I love this language. This pattern of 
generic, math-based abstractions and their wide usage to great effect is 
something that simply cannot be done in a language like Ruby. The 
strictness and expressiveness of Haskell's type system is what allows it 
to be possible.

One such abstraction which shares those qualities but is a bit more 
accessible than Monad is Monoid. In this post, I'd like to explain what 
a Monoid is, describe some concrete use cases, and attempt to show that 
such an abstraction, if attempted in Ruby, would not be nearly as 
useful. Hopefully, this can be a good first step towards more powerful 
ideas like Functor, Applicative, and eventually Monad which I would 
consider to be in the same class of abstractions.

## Associative Identity

Let's play a game. Not a very fun one, but a game nonetheless. You try 
to guess an *operation* and a *value* which satisfies the following 
description:

1. The *operation* is binary, it takes two arguments.
2. The two arguments to and return value from *operation* are the same 
   type as *value*.
3. `(a operation b) operation c` will give you the same answer as `a 
   operation (b operation c)`.
4. `(x operation value)` will always give you `x`. Likewise, `(value 
   operation x)` must always give you `x`.

So, the first two are just a word-heavy way of stating the types of 
*operation* and *value*. The second two are known as the "Monoid Laws".
The first says that if you're going to glue multiple calls of 
*operation* together, the way you associate things doesn't matter and 
second states that using *value* with *operation* acts as "Identity".

So, can you think of some operations and values that satisfy this?

*Insert Final Jeopardy music...*

* For `Int`s there are actually two answers: `+`/`0`, and `*`/`1`.

We can see why:

```haskell
(1 + 2) + 3 -- 6
1 + (2 + 3) -- 6

1 + 0 -- 1
0 + 1 -- 1

(3 * 4) * 5 -- 60
3 * (4 * 5) -- 60

3 * 1 -- 3
1 * 3 -- 3
```

* For `String`s there is `++` (concatenation) and `""` (the empty 
  string).

```haskell
("Hello " ++ "wor") ++ "ld" -- Hello world
"Hello " (++ "wor" ++ "ld") -- Hello world

"Foo" ++ "" -- Foo
"" ++ "Foo" -- Foo
```

`String`s are actually just lists of `Char`s, so we can be more general 
and say that for lists containing any type (`[a]`), there is `++` and 
`[]`.

```haskell
([a, b] ++ [c, d]) ++ [e, f] -- [a, b, c, d, e, f]
[a, b] ++ ([c, d] ++ [e, f]) -- [a, b, c, d, e, f]

[a, b] ++ [] -- [a, b]
[] ++ [a, b] -- [a, b]
```

So there you have it, `Monoid` in a nutshell. Not nearly as hard as 
`Monad` but still interesting in the same way. This can also serve as a 
nice base from which to (eventually) discuss `Monad`.

In Haskell, we call *operation* `mappend` and *value* `mempty`. Thanks 
to the Haskell type system, we can call these functions and, depending 
on the types we give them, the correct operations and values will be 
used:

```haskell
"Hello" `mappend` " world" -- Hello world

1 `mappend` 2 -- No instance...
```

Why didn't that last one work? Well, remember that for numbers there are 
two equally valid instances for `Monoid`: `+`/`0` and `*`/`1`. To solve 
this ambiguity, Haskell uses something called `newtype`s which we won't 
get into today, just note that you need to say which numeric 
implementation you're looking for.

```haskell
Sum 1 `mappend` Sum 2 -- Sum 3

Product 3 `mappend` Product 4 -- Product 12
```

Booleans have a similar requirement, since they also have two equally 
valid concepts: `&&`/`True` or `||`/`False`.

```haskell
Any True `mappend` Any False -- True

All True `mappend` All False -- False
```

I'll bet you didn't expect `Bool` to be a `Monoid`, did you?

## Separation of Concerns

Imagine you're writing an application (in Ruby) where you need to 
perform a number of operations, maybe with a remote API,  and you'd like 
to keep a "transaction" log of those operations. The operations you're 
doing and maintaining this transaction log are clearly separate 
concerns. As a Good Little Programmer, you'd like to separate that out:

```ruby
class TaskRunner
  attr_reader :results

  def initialize
    @results = []
  end

  def perform(&block)
    result << yield

    @results << [:ok, result]
  rescue ex
    @results << [:error, ex.message]
  end
end

runner = TaskRunner.new

runner.perform do
  # Some cool stuff

  "Did some cool stuff"
end

runner.perform do
  # Other cool stuff

  "Did other cool stuff"
end

runner.results
# => [[:ok, "..."], [:ok, "..."]]
```

I don't know about you, but I wouldn't think to push `TaskRunner` up to 
rubygems.org. I wouldn't attempt to extract it out any further than just 
this separate class. Why is that?

The logic is pretty domain specific. The `perform` method is going to 
return some value, and we're going to naively accumulate those into some 
list that our callers can then access later. This is a terrible 
interface from a generic usefulness standpoint. Could things be 
improved? Sure. But I doubt you'd ever get it to a point where you might 
think to release `TaskRunner` as a library.

Imagine later, in the same app even, you're making some kind of 
financial module. It performs a series of financial movements which are 
meant to accumulate some running total dollar value. This sounds oddly 
similar, but just different enough that `TaskRunner` is not a great fit. 
Sure we could have our operations return some dollar, then sum up (or 
product out) the `results` array ourselves after. It means we need to 
get rid of the `:ok`/`:error` tuples, but oh well, we can do that.

What if in stead, task runner were able to be defined like this:

```ruby
class TaskRunner
  def initialize
    @results = mempty
  end

  def perform(&block)
    @results.mappend(yield)
  end
end
```

One might not think it would be possible for the return type of the 
*caller* of `perform` to dictate which implementation of `mempty` would 
be used in the above `initialize`. But that's exactly how Haskell works. 
Haskell would notice that `@results.mappend` is called with whatever's 
returned by `yield`. It would go find the caller of `yield` and see what 
type it's returning. This would tell us what type `@results` has to be, 
which tells us which implementation of `mempty` (and `mappend`) we must 
use. Crazy-pants.

Under this system, the first use case (the transaction log) would simply 
return some type for which `mempty` was an empty log and `mappend` added 
transactions to it. It could literally be any type at all that's in the 
`Monoid` typeclass. The financial module could then return some type for 
which `mempty` was $0.00 and `mappend` was `+`. The `TaskRunner` would 
have zero knowledge of --or dependency on-- these implementations; it 
would only know that it can initialize some thing using `mempty` and 
accumulate results using `mappend`.

Now we've got something generally useful and worth publishing -- in 
fact, we've got a (partial) implementation of [Writer][].

[writer]: http://hackage.haskell.org/package/mtl-2.0.1.0/docs/Control-Monad-Writer-Lazy.html#g:2
