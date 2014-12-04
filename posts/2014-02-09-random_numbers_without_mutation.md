---
title: Random Numbers without Mutation
tags: haskell
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

For the Haskell examples, I'll be using `System.Random.StdGen` in place 
of the `RGen` class we've been working with so far. It is exactly like 
our `RGen` class above in that it can be initialized with some seed, and 
there is a `random` function with the type `StdGen -> (Int, StdGen)`.


The abstract thing we're lacking is a way to call those function 
successively, passing the `StdGen` returned from one invocation as the 
argument to the next invocation, all the while being able to access that 
`a` (the random integer or experiment outcome) whenever needed. Haskell, 
has just such an abstraction, it's in `Control.Monad.State`.

First we'll need some imports.

```haskell
import System.Random
import Control.Monad.State
```

Notice that we have a handful of functions with similar form.

```haskell
(StdGen -> (a, StdGen))
```

What `Control.Monad.State` provides is a type that looks awfully 
similar.

```haskell
data State s a = State { runState :: (s -> (a, s)) }
```

Let's declare a type synonym which fixes that `s` type variable to the 
state we care about: a random number generator.

```haskell
type RGenState a = State StdGen a
```

By replacing the `s` in `State` with our `StdGen` type, we end up with a 
more concrete type that looks as if we had written this:

```haskell
data RGenState a = RGenState
    { runState :: (StdGen -> (a, StdGen)) }
```

And then went on to write all the various instances that make this type 
useful. By using such a type synonym, we get all those instances and 
functions for free.

Our first example:

```haskell
rand :: RGenState Int
rand = state random
```

We can "evaluate" this action with one of a number of functions provided 
by the library, all of which require some initial state. `runState` will 
literally just execute the function and return the result and the 
updated state (in case you missed it, it's just the record accessor for 
the `State` type). `evalState` will execute the function, discard the 
updated state, and give us only the result. `execState` will do the 
inverse: execute the function, discard the result, and give us only the 
updated state.

We'll be using `evalState` exclusively since we don't care about how the 
random number generator ends up after these actions, only that it gets 
updated and passed along the way. Let's wrap that up in a function that 
both provides the initial state and evaluates the action.

```haskell
runRandom :: RGenState a -> a
runRandom f = evalState f (mkStdGen 1)

-- runRandom rand
-- => 7917908265643496962
```

Unfortunately, the result will be the same every time since we're using 
a constant seed. You'll see soon that this is an easy limitation to 
address after the fact.

With this bit of glue code in hand, we can re-write our program in a 
nice modular way without any actual mutable state or re-assignment.

```haskell
estimatePi :: Int -> Double
estimatePi n = sqrt $ 6 / (monteCarlo n cesaro)

cesaro :: RGenState Bool
cesaro = do
    x1 <- rand
    x2 <- rand

    return $ gcd x1 x2 == 1

monteCarlo :: Int -> RGenState Bool -> Double
monteCarlo trials experiment = runRandom $ do
    outcomes <- replicateM trials experiment

    return $ (length $ filter id outcomes) `divide` trials

  where
    divide :: Int -> Int -> Double
    divide a b = fromIntegral a / fromIntegral b
```

Even with a constant seed, it works pretty well:

```haskell
main = print $ estimatePi 1000
-- => 3.149183286488868
```

## And For My Last Trick

It's easy to fall into the trap of thinking that Haskell's type system 
is limiting in some way. The `monteCarlo` function above can only work 
with random-number-based experiments? Pretty weak.

Consider the following refactoring:

```haskell
estimatePi :: Int -> RGenState Double
estimatePi n = do
  p <- monteCarlo n cesaro

  return $ sqrt (6 / p)

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
    divide :: Int -> Int -> Double
    divide a b = fromIntegral a / fromIntegral b

main :: IO ()
main = print $ runRandom $ estimatePi 1000
```

The minor change made was moving the call to `runRandom` all the way up 
to `main`. This allows us to pass stateful computations throughout our 
application without ever caring about that state except at this highest 
level.

This would make it simple to add true randomness (which requires `IO`) 
by replacing the call to `runRandom` with something that pulls entropy 
in via `IO` rather than using `mkStdGen`.

```haskell
runTrueRandom :: RGenState a -> IO a
runTrueRandom f = do
    s <- newStdGen

    evalState f s

main = print =<< runTrueRandom (estimatePi 1000)
```

One could even do this conditionally so that your random-based 
computations became deterministic during tests.

Another important point here is that `monteCarlo` can now work with *any 
Monad!* This makes perfect sense: The purpose of this function is to run 
experiments and tally outcomes. The idea of an *experiment* only makes 
sense if there's some outside force which might change the results from 
run to run, *but who cares what that outside force is?* Haskell don't 
care. Haskell requires we only specify it as far as we need to: it's 
some Monad `m`, nothing more.

This means we can run IO-based experiments via the Monte Carlo method 
with the same `monteCarlo` function just by swapping out the monad:

What if Cesaro claimed the probability that the current second is an 
even number is equal to 6/&pi;<sup>2</sup>? Seems reasonable, let's 
model it:

```haskell
-- same code, different name / type
estimatePiIO :: Int -> IO Double
estimatePiIO n = do
  p <- monteCarlo n cesaroIO

  return $ sqrt (6 / p)

cesaroIO :: IO Bool
cesaroIO = do
  t <- getCurrentTime

  return $ even $ utcDayTime t

monteCarlo :: Monad m => Int -> m Bool -> m Double
monteCarlo trials experiment = -- doesn't change at all!

main :: IO ()
main = print =<< estimatePiIO 1000
```

I find the fact that this expressiveness, generality, and polymorphism 
can share the same space as the strictness and incredible safety of this 
type system fascinating.
