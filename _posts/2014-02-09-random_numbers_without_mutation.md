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
write our estimation function like so:

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

This process is a generic concern which has nothing to do with Cesaro's 
method or performing Monte Carlo tests. We should be able to separate it 
out of our main algorithm. Haskell's State Monad allows us to do exactly 
that.

## RGenState

Haskell provides a parameterized type, `State s a` which has a `Monad` 
instance. With a simple type synonym, we can represent the state of our 
random number generator as something we can treat as a Monad.

```haskell
import Control.Monad.State
import System.Random

-- System.Random.StdGen is basically our RGen class
type RGenState a = State StdGen a

withRGen :: RGenState a -> a
withRGen f = evalState f (mkStdGen 1)
```

And exactly like one might write a `readFile` function in the `IO` Monad 
to access the outside world and provide you a file's contents, we can 
write a `rand` function in the `RGenState` Monad which accesses the 
random number generator to return you a random number.

```haskell
rand :: RGenState Int
rand = do
  (i, g) <- gets next -- call next on current generator
  put g               -- set the new generator as the post-action state

  return i            -- produce the random number as the result
```

With all this in place, we have the following application code:

```haskell
estimatePi :: Int -> Double
estimatePi n = sqrt $ 6 / (monteCarlo n cesaro)

cesaro :: RGenState Bool
cesaro = do
  x1 <- rand
  x2 <- rand

  return $ gcd x1 x2 == 1

monteCarlo :: Int -> RGenState Bool -> Double
monteCarlo trials experiment = withRGen $ do
  outcomes <- replicateM trials experiment

  return $ (length $ filter id outcomes) `divide` trials

  where
    -- divide integers as floats
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
