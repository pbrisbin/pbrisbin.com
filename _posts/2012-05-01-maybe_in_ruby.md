---
layout: post
title: "Maybe In Ruby"
tags:
  - ruby
  - haskell
---

Sometimes it's fun to do something completely useless.

Recently, I wrote a [post][] about how awesome the Maybe type 
is in Haskell. In the post, I talked about Functors and Monads and how 
Maybe can help us understand them.

[post]: /posts/maybe_is_just_awesome "Maybe is Just Awesome"

Shortly thereafter, I was bored on the train one day and decided to 
implement Maybe and its functor instance in ruby.

<div class="well">
In this post I'll be relying on the fact that `obj.(args)` is translated 
to `obj.call(args)` in newer rubies. I find it makes the example read 
better.
</div>

## Maybe

So we need an object that can represent "Just something" or "Nothing". 
Ruby already has the concept of `nil`, so we'll piggy back on that and 
just wrap it all in some sugar.

```ruby 
class Maybe
  def initialize(value)
    @value = value
  end

  def nothing?
    @value.nil?
  end

  def just?
    !nothing?
  end

  def value
    if just?
      @value
    else
      raise "Can't get something from nothing."
    end
  end

  # we'll need this to prove the laws
  def ==(other)
    if just? && other.just?
      return value == other.value
    end

    nothing? && other.nothing?
  end
end

def Just(x)
  raise "Can't make something from nothing." if x.nil?

  Maybe.new(x)
end

Nothing = Maybe.new(nil)
```

## Functions

We can't map functions to methods because methods need targets, they 
can't stand on their own. As an example, take `id` (which we'll be using 
later on). One might be tempted to define it like this:

```ruby 
def id(x)
  x
end
```

This won't work for our purposes since that method (defined on the 
global object `Object`) can't be passed around, partially applied or 
composed.

It's more convenient to do it like this:

```ruby 
# ruby 1.9
id = ->(x) { x }

# ruby 1.8
id = lambda { |x| x }
```

Now you've got an isolated, callable `id` object which you can pass 
around.

## Partial Application

Functions need to be partially applied. That means you can give a 
function a few of the arguments it expects and get back another function 
which you can then pass around and eventually call with the additional 
arguments given at that later point:

```ruby 
class Partial
  def initialize(f, *args)
    @f, @args = f, args
  end

  def call(*args)
    new_args = @args + args

    @f.(*new_args)
  end
end

def partial(f, *args)
  Partial.new(f, *args)
end

max = ->(x,y) { x >= y ? x : y }

max.(4, 5) # => 5

max5 = partial(max, 5)

max5.(6) # => 6
max5.(4) # => 5

[4, 5, 6].map { |i| max5.(i) } # => [5, 5, 6]
```

## Composition

Two functions, when composed together, return a new function which 
represents the first being applied to the result of the second being 
applied to the argument given.

```ruby 
class Compose
  def initialize(f, g)
    @f, @g = f, g
  end

  def call(x)
    @f.( @g.( x ) )
  end
end

def compose(f, g)
  Compose.new(f, g)
end

get_len = ->(s) { s.length   }
add_bar = ->(s) { s + "_bar" }

get_len_with_bar = compose(get_len, add_bar)

get_len_with_bar.("foo") # => 7
```

<div class="well">
This is all **so much easier** in Haskell...
</div>

## Functor

Now that we can define functions, partially apply them and compose them 
together, we can finally prove the Functor laws for our new `Maybe` 
class.

Let's start by defining `fmap`, just as it is in Haskell:

```ruby 
# fmap f (Just x) = Just (f x)
# fmap _ Nothing  = Nothing
fmap = ->(f, x) do
  if x.just?
    Just(f.(x.value))
  else
    Nothing
  end
end
```

<div class="well">
Strictly speaking, `fmap`'s behavior is type-dependant. So a real 
implementation (for some definition of "real") would probably make a 
method on `Object` which needs to be overridden by any classes that are 
proper Functors. We won't worry about that here...
</div>

First law, the identity operation must behave the same when it's 
`fmap`ped.

```ruby 
id = ->(x) { x }

fmap_id = partial(fmap, id)

# fmap id = id
fmap_id.(Nothing)     == id.(Nothing)     # => true
fmap_id.(Just("foo")) == id.(Just("foo")) # => true
```

So far so good.

Second law, `fmap`ping a composed function is no different than 
composing the result of each function `fmap`ped separately.

```ruby 
f = ->(s) { s + "_bar" }
g = ->(s) { s.length   }

f_g = compose(f, g)

fmap_f_g = partial(fmap, f_g)

fmap_f = partial(fmap, f)
fmap_g = partial(fmap, g)

fmap_f_fmap_g = compose(fmap_f, fmap_g)

# fmap (f . g) == fmap f . fmap g
fmap_f_g.(Nothing)     == fmap_f_fmap_g.(Nothing)    # => true
fmap_f_g.(Just("foo")) == fmap_f_fmap_g.(Just("foo") # => true
```

As suspected, our new Ruby-Maybe is a proper Functor.

## Monad?

Is our class a Monad?

```ruby 
# >>=
f = ->(ma, f) do
  if ma.just?
    f.(ma.value)
  else
    Nothing
  end
end

# >>
f = ->(ma, mb) do
  if ma.just?
    mb
  else
    Nothing
  end
end

# return
f = ->(x) do
  Just(x)
end

# fail
f = -> do
  Nothing
end
```

Proving the laws is left as an exercise to the reader...
