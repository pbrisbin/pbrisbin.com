---
layout: post
title: "Random Numbers without Mutation"
tags:
  - haskell
---

In [lecture 5A][5a] of Structure & Interpretation of Computer Programs, 
Gerald Sussman introduces the idea of assignments, side effects and 
state. Before that, they had been working entirely in purely functional 
Lisp which could be completely evaluated and reasoned about using the 
substitution model. He states repeatedly that this is a horrible thing 
as it requires a far more complex view of programs. At the end of the 
lecture, he shows a compelling example of why we must introduce this 
horrible thing anyway; without it, we cannot decouple parts of our 
algorithms cleanly and would be reduced to huge single-function programs 
in some critical cases.

[5a]: http://www.youtube.com/watch?v=wUqUvTp30XE

The example chosen in SICP is estimating &pi; using Cesaro's method. The 
method states that the probability that any two random numbers' greatest 
common divisor equals 1 is itself equal to 6/&pi;<sup>2</sup>.

Since I know Ruby better than Lisp (and I'd venture my readers do too), 
here's a ported version:

```ruby
def estimate_pi(trials)
  p = monte_carlo(trials) { cesaro }

  Math.sqrt(6 / p)
end

def cesaro
  rand.gcd(rand) == 1
end

def monte_carlo(trials, &block)
  iter = ->(trials, passed) do
    if trials == 0
      passed
    else
      if block.call
        iter.call(trials - 1, passed + 1)
      else
        iter.call(trials - 1, passed)
      end
    end
  end

  iter.call(trials, 0) / trials.to_f
end
```

I've written this code to closely match the Lisp version which used a 
recursive iterator. Unfortunately, this means that any reasonable number 
of trials will exhaust Ruby's stack limit.

The code above also assumes a `rand` function which will return 
different random integers on each call. To do so, it must employ 
mutation and hold internal state:

```ruby
def rand
  @x ||= random_init
  @x   = random_update(@x)

  @x
end
```

Here I assume the same primitives as Sussman does, though it wouldn't be 
difficult to wrap Ruby's built-in `rand` to return integers instead of 
floats. The important thing is that this function needs to hold onto the 
previously returned random value in order to provide the next.

Sussman states that without this impure `rand` function, it would be 
very difficult to decouple the `cesaro` function from the `monte_carlo` 
one. Without utilizing (re)assignment and mutation, we would have to 
write our estimation function as one giant blob:

```ruby
def estimate_pi(trials)
  iter = ->(trials, passed, x1, x2) do
    if trials == 0
      passed
    else
      x1_ = rand_update(x2)
      x2_ = rand_update(x1_)

      if x1.gcd(x2) == 1
        iter.call(trials - 1, passed + 1, x1_, x2_)
      else
        iter.call(trials - 1, passed, x1_, x2_)
      end
    end
  end

  x1 = rand_init
  x2 = rand_update(x1)

  p = iter.call(trials, 0, x1, x2) / trials.to_f

  Math.sqrt(6 / p)
end
```

Ouch.

It's at this point Sussman stops, content with his justification for 
adding mutability to Lisp. I'd like to explore a bit further: what if 
remaining pure were non-negotiable? Are there other ways to make 
decoupled systems and elegant code without sacrificing purity?

## RGen

Let's start with a non-mutating random number generator:

```ruby
class RGen
  def initialize(seed = nil)
    @seed = seed || random_init
  end

  def next
    x = random_update(@seed)

    [x, RGen.new(x)]
  end
end

def rand(g)
  g.next
end
```

This allows for the following implementation:

```ruby
def estimate_pi(trials)
  p = monte_carlo(trials) { |g| cesaro(g) }

  Math.sqrt(6 / p)
end

def cesaro(g)
  x1, g1 = rand(g)
  x2, g2 = rand(g1)

  [x1.gcd(x2) == 1, g2]
end

def monte_carlo(trials, &block)
  iter = ->(trials, passed, g) do
    if trials == 0
      passed
    else
      ret, g_ = block.call(g)

      if ret
        iter.call(trials - 1, passed + 1, g_)
      else
        iter.call(trials - 1, passed, g_)
      end
    end
  end

  iter.call(trials, 0, RGen.new) / trials.to_f
end
```

We've moved out of the single monolithic function, which is a step in 
the right direction. The additional generator arguments being passed all 
over the place makes for some readability problems though. The reason 
for that is a missing abstraction; one that's difficult to model in 
Ruby. To clean this up further, we'll need to move to a language where 
purity was in fact non-negotiable: Haskell.

In Haskell, the type signature of our current `monte_carlo` function 
would be:

```haskell
monteCarlo :: Int                    -- number of trials
           -> (RGen -> (Bool, RGen)) -- the experiment
           -> Double                 -- result
```

Within `monte_carlo`, we need to repeatedly call the block with a fresh 
random number generator. Calling `RGen#next` gives us an updated 
generator along with the next random value, but that must happen within 
the iterator block. In order to get it out again and pass it into the 
next iteration, we need to return it. This is why `cesaro` has the type 
that it does:

```haskell
cesaro :: RGen -> (Bool, RGen)
```

`cesaro` depends on some external state so it accepts it as an argument. 
It also affects that state so it must return it as part of its return 
value. `monteCarlo` is responsible for creating an initial state and 
"threading" it though repeated calls to the experiment given. Mutable 
state is "faked" by passing a return value as argument to each 
computation in turn.

You'll also notice this is a similar type signature as our `rand` 
function:

```haskell
rand :: RGen -> (Int, RGen)
```

This similarity and process is a generic concern which has nothing to do 
with Cesaro's method or performing Monte Carlo tests. We should be able 
to leverage the similarities and separate this concern out of our main 
algorithm. Monadic state allows us to do exactly that.

## RGenState

Before I get into `RGenState`, note that in the following I'll be using 
`System.Random.StdGen` in place of the `RGen` class we've been working 
with so far. It is exactly like our `RGen` class above in that it can be 
initialized with some seed, and there is a `next` function with the type 
`StdGen -> (Int, StdGen)`. On to the Monads...

So we've got a handful of functions with the following basic type:

```haskell
(StdGen -> (a, StdGen))
```

The abstract thing we're lacking is a way to call those function 
successively, passing the `StdGen` returned from one invocation as the 
argument to the next invocation, all the while being able to access that 
`a` (the random integer or experiment outcome) whenever needed. Haskell, 
has just such an abstraction, it's in `Control.Monad.State`.

Rather than use that here (which would be very easy and probably advised 
in a "Real World" setting), I'm going to recreate it specifically for 
our `StdGen` type.

First we need to give this thing a name:

```haskell
import System.Random

newtype RGenState a = RGenState
  { runRandom :: (StdGen -> (a, StdGen)) }

-- re-read the above carefully, until you know why this is basically
--
--   :: (StdGen -> (Int, StdGen))
--
rand :: RGenState Int
rand = RGenState next
```

Now we come to the Monad. The simplest way I can describe a Monad is 
it's an interface which describes a way to compose multiple values of 
types with context into a single value of the same type with the 
appropriate context.

For example, if you've got two functions which have the type signature 
above (they take a random number generator and return a new one), the 
`StdGen` involved is the context. To compose two (or more) of these 
functions means to call them successively while feeding the generator 
returned by one into the other -- which is exactly what we need to do.

So let's describe that by making our new type an instance of `Monad`:

```haskell
instance Monad RGenState where
    (>>=) a f = RGenState $ \s ->
        let (x, s2) = runRandom a s
        in runRandom (f x) s2

    return x = RGenState $ \s -> (x, s)
```

I feel your eyes glazing over. Let's break this down:

```haskell
instance Monad RGenState where
```

I'm saying that my type (`RGenState`) can be treated just like any other 
Monad. To do this, I must satisfy some conditions, I satisfy them 
because...

```haskell
    (>>=) a f = 
```

The monadic bind function (`(>>=)`) for my type, when given arguments 
`a` (a value of my type) and `f` (a function from a normal value to one 
of my type) is implemented as follows...

```haskell
    (>>=) a f = RGenState
```

My type constructor (so I'm building a value of my type)...

```haskell
    (>>=) a f = RGenState $ \s ->
```

Given a function of one argument, `s` which...

```haskell
        let (x, s2) = runRandom a s
```

Calls the stateful computation of the value `a` with that initial state 
`s` and sets the result to `x` and the new state to `s2`, then...

```haskell
        in runRandom (f x) s2
```

Calls the function `f` on the normal value `x` to produce a new stateful 
computation which I then execute on that new state, `s2`.

```haskell
    return x = 
```

I also must say that any normal value `x` can be made into a stateful 
computation simply by...


```haskell
    return x = RGenState
```

Supplying my constructor...

```haskell
    return x = RGenState $ \s ->
```

With a function of one argument, `s`, which...

```haskell
    return x = RGenState $ \s -> (x, s)
```

Just returns that value with the state unmodified.

Holy cow. Talking about that takes *way* longer than just coding it.

## withRGen

That takes care of composition. We'll also need some function which 
evaluates a stateful computation on some initial state:

```haskell
withRGen :: RGenState a -> a
withRGen f = fst $ runRandom f $ mkStdGen 1
```

This one's pretty easy. `runRandom f` takes the `RGenState` value, and 
extracts the actual function (the one of type `(RGen -> (a, RGen)`). It 
then immediately calls that function on the initial random number 
generator (`mkStdGen 1`) and discards the resulting state (`fst`) 
returning just the value.

With all this in place, we have the following application code:

```haskell
estimatePi :: Int -> Double
estimatePi n = sqrt $ 6 / (monteCarlo n cesaro)

cesaro :: RGenState Bool
cesaro = do
  -- see how easy it is to get at the `a` value?
  x1 <- rand
  x2 <- rand

  return $ gcd x1 x2 == 1

monteCarlo :: Int -> RGenState Bool -> Double
monteCarlo trials experiment = withRGen $ do
  -- and how easy it is to compose multiple calls?
  outcomes <- replicateM trials experiment

  return $ (length $ filter id outcomes) `divide` trials

  where
    divide :: Int -> Int -> Double
    divide a b = fromIntegral a / fromIntegral b
```

Here we see a concise, readable (I claim anyway), and modular algorithm 
for estimating &pi;. It's true that the various functions are coupled by 
way of the `RGenState` type annotation which is not the case with the 
mutable version, but I'd argue that's a feature.

Our generic `monteCarlo` was always meant to work only with experiments 
which require access to random numbers and return true or false. That 
fact has now been made explicit in the code and is enforced by the type 
system.

It works pretty well too:

```haskell
main = print $ estimatePi 100000
-- => 3.1368931127763995
```

## And For My Last Trick

It's easy to fall into the trap of thinking that Haskell's type system 
is limiting in some way. The `monteCarlo` function above can only work 
with random-number-based experiments? That's weak.

Not so fast.

Remember that `RGenState` is just another Monad. Why is being a Monad a 
useful thing? If code is not relying on you, but only on the fact that 
you're a Monad, that means you can be swapped out for any other Monad.

For example, suppose the following refactoring:

```haskell
estimatePi :: Int -> Double
estimatePi n = sqrt $ 6 / withRGen (monteCarlo n cesaro)

cesaro :: RGenState Bool
cesaro = do
  x1 <- rand
  x2 <- rand

  return $ gcd x1 x2 == 1

monteCarlo :: Monad m => Int -> m Bool -> m Double
monteCarlo trials experiment = do
  outcomes <- replicateM trials experiment

  return $ (length $ filter id outcomes) `divide` trials

  where
    -- unimportant, just divides integers as floats
    divide :: Int -> Int -> Double
    divide a b = fromIntegral a / fromIntegral b
```

The minor change made was moving the call to `withRGen` up into 
`estimatePi`. It's the function which uses `cesaro` which requires 
random numbers, so it can be the one to *evaluate the Monad* which 
requires providing and managing the random number context. You didn't 
realize that concerns could be so well separated, did you?

`monteCarlo` can now work with *any Monad!* This makes perfect sense: 
The purpose of this function is to run experiments and tally outcomes. 
The idea of an *experiment* only makes sense if there's some outside 
force which might change the results from run to run, *but who cares 
what that outside force is?* Haskell don't care. Haskell requires we 
only specify it as far as we need to: it's some Monad `m`, nothing more.

This means we can run IO-based experiments via the Monte Carlo method 
with the same `monteCarlo` function just as easily:

```haskell
estimatePi :: Int -> IO Double
estimatePi n = do
  p <- monteCarlo n cesaroIO

  return $ sqrt (6 / p)

-- What if Cesaro claimed the probability that the current second is an 
-- even number is equal to 6/pi2?
cesaroIO :: IO Bool
cesaroIO = do
  t <- getCurrentTime

  return $ even $ utcDayTime t

monteCarlo :: Monad m => Int -> m Bool -> m Double
monteCarlo trials experiment = -- doesn't change at all!
```

I find the fact that this expressiveness, generality, and polymorphism 
can share the same space as the strictness and incredible safety of this 
type system fascinating.
