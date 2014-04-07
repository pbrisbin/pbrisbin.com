---
layout: post
title: "Regular Expression Evaluation via Finite Automata"
tags:
  - haskell
---

<div class="well">
What follows is a literate haskell file runnable via `ghci`. The raw 
source for this page can be found [here][here].
</div>

[here]: https://github.com/pbrisbin/pbrisbin.com/tree/master/_posts

While reading [Understanding Computation][uc] again last night, I was 
going back through the chapter where Tom Stuart describes deterministic 
and non-deterministic finite automata. These simple state machines seem 
like little more than a teaching tool, but he eventually uses them as 
the implementation for a regular expression matcher. I thought seeing 
this concrete use for such an abstract idea was interesting and wanted 
to re-enforce the ideas by implementing such a system myself -- with 
Haskell, of course.

[uc]: http://computationbook.com/

Before we get started, we'll just need to import some libraries:

> import Control.Monad.State
> import Data.List (foldl')
> import Data.Maybe

<h2>Patterns</h2>

We're going to model a subset of regular expression patterns.

> data Pattern =
>       Empty                   -- ""
>     | Literal Char            -- "a"
>     | Concat Pattern Pattern  -- "ab"
>     | Choose Pattern Pattern  -- "a|b"
>     | Repeat Pattern          -- "a*"

Checking for matches against these patterns means converting them to 
[Deterministic Finite Automata][dfa] or DFAs, then letting the DFAs 
operate on input strings and give us a useful answer.

[dfa]: http://en.wikipedia.org/wiki/Deterministic_finite_automaton

<h3>A Bit About Mutable State</h3>

Since this is a recursive data type, we're going to have to recursively 
create and combine DFAs. For example, in a `Concat` pattern, we'll need 
to turn both sub-patterns into DFAs then combine those in some way. In 
the Ruby implementation, Mr. Stuart used `Object.new` to ensure unique 
state identifiers between all the DFAs he has to create. We can't do 
that in Haskell. There's no global state able to provide some 
guaranteed-unique value.

What we're going to do to get around this is conceptually simple, but 
appears complicated because it makes use of monads. All we're doing is 
defining a list of identifiers at the beginning of our program and 
drawing from that list whenever we need a new identifier. Because we 
can't maintain that as a variable we constantly update every time we 
pull an identifier out, we'll use the `State` monad to mimic mutable 
state through our computations.

<div class="well">
I apologize for the naming confusion here. This `State` type is from the 
Haskell library and has nothing to with the states of our DFAs.
</div>

Our identifiers are just `Int`s, but we'll call then `DFAState`s 
throughout the system.

> type DFAState = Int

We can then take the polymorphic `State s a` type, and fix the `s` 
variable as a list of (potential) identifiers.

> type DSI a = State [DFAState] a

We can now use `evalState` to provide the infinite list of integers as 
the pool to draw from, execute some stateful action, and ultimately 
return the result.

> runDSI :: DSI a -> a
> runDSI f = evalState f [1..]

This makes it simple to create a `nextId` action which requests the next 
identifier from this list as well as updates the computation's state, 
removing it as a future option before presenting that next identifier as 
its result.

> nextId :: DSI DFAState
> nextId = do
>     (x:xs) <- get
>     put xs
>     return x

As long as our program is a single action passed to `runDSI` (which we 
can build incrementally and compose monadically), we're guaranteed to 
get a unique state identifier every time we want one.

Who needs mutable state to write programs?

<h2>DFAs</h2>

DFAs are very simple machines. They have some states and some rules. 
They read characters in and move from state to state according to those 
rules. Some states are special, they're known as "accept" states. What 
we're going to do is construct a DFA whose rules for moving from state 
to state are derived from the nature of the pattern it represents. Only 
if the DFA we construct moves to an accept state for a given string of 
input does it mean the string matches that pattern.

> matches :: String -> Pattern -> Bool
> matches s = (`accepts` s) . runDSI . toDFA

We can test this out in `ghci`:

```
ghci> "" `matches` Empty
True
ghci> "abc" `matches` Empty
False
```

And use it in an example `main`:

> main :: IO ()
> main = do
>     -- This AST represents the pattern /ab|cd*/:
>     let p = Choose
>             (Concat (Literal 'a') (Literal 'b'))
>             (Concat (Literal 'c') (Repeat (Literal 'd')))
>
>     print $ "xyz" `matches` p
>     -- => False
>
>     print $ "cddd" `matches` p
>     -- => True

<h2>Representing a DFA</h2>

A DFA is a machine with a set of rules, one or more current states (to 
handle non-determinism), and one or more accept states.

> data DFA = DFA
>     { rules         :: [Rule]
>     , currentStates :: [DFAState]
>     , acceptStates  :: [DFAState]
>     } deriving Show

A rule defines what characters tell the machine to change states and 
which state to move into.

> data Rule = Rule
>     { fromState  :: DFAState
>     , inputChar  :: Maybe Char
>     , nextStates :: [DFAState]
>     } deriving Show

The reason `inputChar` and `nextStates` are not single values is because 
we need to use this deterministic machine to model a non-deterministic 
one. It's possible (and required, in fact) to have a rule like *If in 
State 1 and an "a" is read, go to State 2 or State 3*. We can model that 
by having both 2 and 3 in `nextStates` for that rule. It's also possible 
(and also required) to have such a thing as a "Free Move". This means 
that the machine can change states without reading any input. This is 
useful if there is a state reachable via a Free Move which has a normal 
rule for the character about to be read. The machine can "jump" to that 
state, then follow that rule. These are modelled here by a `Nothing` 
value in the `inputChar` field.

If, after processing some input, any of the machine's current states are 
in its list of "accept" states, the machine has accepted the input.

> accepts :: DFA -> [Char] -> Bool
> accepts dfa = accepted . foldl' process dfa
>
>   where
>     accepted :: DFA -> Bool
>     accepted dfa = any (`elem` acceptStates dfa) (currentStates dfa)

Processing a single character means finding any followable rules for the 
given character and the current machine state, and following them.

> process :: DFA -> Char -> DFA
> process dfa c = case findRules c dfa of
>     -- Invalid input should cause the DFA to go into a failed state. 
>     -- We can do that easily, just remove any acceptStates.
>     [] -> dfa { acceptStates = [] }
>     rs -> dfa { currentStates = followRules rs }
>
> findRules :: Char -> DFA -> [Rule]
> findRules c dfa = filter (ruleApplies c dfa) $ rules dfa

A rule applies if

1. The read character is a valid input character for the rule, and
2. That rule applies to an available state

> ruleApplies :: Char -> DFA -> Rule -> Bool
> ruleApplies c dfa r =
>     maybe False (c ==) (inputChar r) &&
>     fromState r `elem` availableStates dfa

An "available" state is one which we're currently in, or can reach via 
Free Moves.

> availableStates :: DFA -> [DFAState]
> availableStates dfa = currentStates dfa ++ freeStates dfa

The process of finding free states (those reachable via Free Moves) gets 
a bit hairy. We need to start from our current state(s) and follow any 
Free Moves *recursively*. This ensures that Free Moves which lead to 
other Free Moves are correctly accounted for.

> freeStates :: DFA -> [DFAState]
> freeStates dfa = go [] (currentStates dfa)
>
>   where
>     go acc [] = acc
>     go acc ss =
>         let ss' = followRules $ freeMoves dfa ss
>         in go (acc ++ ss') ss'

Free Moves from a given set of states are rules for those states which 
have no input character.

> freeMoves :: DFA -> [DFAState] -> [Rule]
> freeMoves dfa ss = filter (\r ->
>     (fromState r `elem` ss) && (isNothing $ inputChar r)) $ rules dfa

Of course, the states that result from following rules are simply the 
concatenation of those rules' next states.

> followRules :: [Rule] -> [DFAState]
> followRules = concatMap nextStates

Now we can model a DFA and see if it accepts a string or not. You could 
test this in `ghci` by defining a DFA in state 1 with an accept state 2 
and a single rule that moves the machine from 1 to 2 if the character 
"a" is read.

```
ghci> let dfa = DFA [Rule 1 (Just 'a') [2]] [1] [2]
ghci> dfa `accepts` "a"
True
ghci> dfa `accepts` "b"
False
```

Pretty cool.

<h2>Pattern &rArr; DFA</h2>

Our conversion function, `toDFA` will live in the `DSI` monad, allowing 
it to call `nextId` at will. This gives it the following type signature:

> toDFA :: Pattern -> DSI DFA

Every pattern is going to need at least one state identifier, so we'll 
pull that out first, then begin a case analysis on the type of pattern 
we're dealing with:

> toDFA p = do
>     s1 <- nextId
>
>     case p of

The empty pattern results in a predictably simple machine. It has one 
state which is also an accept state. It has no rules. If it gets any 
characters, they'll be considered invalid and put the machine into a 
failed state. Giving it no characters is the only way it can remain in 
an accept state.

>         Empty -> return $ DFA [] [s1] [s1]

Also simple is the literal character pattern. It has two states and a 
rule between them. It moves from the first state to the second only if 
it reads that character. Since the second state is the only accept 
state, it will only accept that character.

>         Literal c -> do
>             s2 <- nextId
>
>             return $ DFA [Rule s1 (Just c) [s2]] [s1] [s2]

We can model a concatenated pattern by first turning each sub-pattern 
into their own DFAs, and then connecting the accept state of the first 
to the start state of the second via a Free Move. This means that as the 
combined DFA is reading input, it will only accept that input if it 
moves through the first DFAs states into what used to be its accept 
state, hop over to the second DFA, then move into its accept state. 
Conceptually, this is exactly how a concatenated pattern should match.

*Note that `freeMoveTo` will be shown after.*

>         Concat p1 p2 -> do
>             dfa1 <- toDFA p1
>             dfa2 <- toDFA p2
>
>             let freeMoves = map (freeMoveTo dfa2) $ acceptStates dfa1
>
>             return $ DFA
>                 (rules dfa1 ++ freeMoves ++ rules dfa2)
>                 (currentStates dfa1)
>                 (acceptStates dfa2)

We can implement choice by creating a new starting state, and connecting 
it to both sub-patterns' DFAs via Free Moves. Now the machine will jump 
into both DFAs at once, and the composed machine will accept the input 
if either of the paths leads to an accept state.

>         Choose p1 p2 -> do
>             s2 <- nextId
>             dfa1 <- toDFA p1
>             dfa2 <- toDFA p2
>
>             let freeMoves =
>                     [ freeMoveTo dfa1 s2
>                     , freeMoveTo dfa2 s2
>                     ]
>
>             return $ DFA
>                 (freeMoves ++ rules dfa1 ++ rules dfa2) [s2]
>                 (acceptStates dfa1 ++ acceptStates dfa2)
>

A repeated pattern is probably hardest to wrap your head around. We need 
to first convert the sub-pattern to a DFA, then we'll connect up a new 
start state via a Free Move (to match 0 occurrences), then we'll connect 
the accept state back to the start state (to match repetitions of the 
pattern).

>         Repeat p -> do
>             s2 <- nextId
>             dfa <- toDFA p
>
>             let initMove = Rule s2 Nothing (currentStates dfa)
>                 freeMoves = map (freeMoveTo dfa) $ acceptStates dfa
>
>             return $ DFA
>                 (initMove : rules dfa ++ freeMoves) [s2]
>                 (acceptStates dfa ++ [s2])
>

And finally, our little helper which connects some state up to a DFA via 
a Free Move.

>   where
>     freeMoveTo :: DFA -> DFAState -> Rule
>     freeMoveTo dfa s = Rule s Nothing (currentStates dfa)

<h2>That's It</h2>

I want to give a big thanks to Tom Stuart for writing Understanding 
Computation. That book has opened my eyes in so many ways. I understand 
why he chose Ruby as the book's implementation language, but I find 
Haskell to be better-suited to these sorts of modeling tasks. Hopefully 
he doesn't mind me exploring that by rewriting some of his examples.
