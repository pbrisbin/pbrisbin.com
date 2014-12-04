---
title: Regular Expression Evaluation via Finite Automata
tags: haskell
---

<div class="well">
What follows is a literate haskell file runnable via `ghci`. The raw 
source for this page can be found [here][here].
</div>

[here]: https://github.com/pbrisbin/pbrisbin.com/blob/master/_posts/2014-04-07-regular_expression_evaluation_via_finite_automata.lhs

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

<h2>Patterns and NFAs</h2>

We're going to model a subset of regular expression patterns.

> data Pattern
>     = Empty                   -- ""
>     | Literal Char            -- "a"
>     | Concat Pattern Pattern  -- "ab"
>     | Choose Pattern Pattern  -- "a|b"
>     | Repeat Pattern          -- "a*"
>     deriving Show

With this, we can build "pattern ASTs" to represent regular expressions:

```
ghci> let p = Choose (Literal 'a') (Repeat (Literal 'b')) -- /a|b*/
```

It's easy to picture a small parser to build these out of strings, but 
we won't do that as part of this post. Instead, we'll focus on 
converting these patterns into [Nondeterministic Finite Automata][nfa] 
or NFAs. We can then use the NFAs to determine if the pattern matches a 
given string.

[nfa]: http://en.wikipedia.org/wiki/Nondeterministic_finite_automaton

To explain NFAs, it's probably easiest to explain DFAs, their 
deterministic counter parts, first. Then we can go on to describe how 
NFAs differ.

A DFA is a simple machine with states and rules. The rules describe how 
to move between states in response to particular input characters. 
Certain states are special and flagged as "accept" states. If, after 
reading a series of characters, the machine is left in an accept state 
it's said that the machine "accepted" that particular input.

An NFA is the same with two notable differences: First, an NFA can have 
rules to move it into more than one state in response to the same input 
character. This means the machine can be in more than one state at once. 
Second, there is the concept of a Free Move which means the machine can 
jump between certain states without reading any input.

Modeling an NFA requires a type with rules, current states, and accept 
states:

> type SID = Int -- State Identifier
>
> data NFA = NFA
>     { rules         :: [Rule]
>     , currentStates :: [SID]
>     , acceptStates  :: [SID]
>     } deriving Show

A rule defines what characters tell the machine to change states and 
which state to move into.

> data Rule = Rule
>     { fromState  :: SID
>     , inputChar  :: Maybe Char
>     , nextStates :: [SID]
>     } deriving Show

Notice that `nextStates` and `currentStates` are lists. This is to 
represent the machine moving to, and remaining in, more than one state 
in response to a particular character. Similarly, `inputChar` is a 
`Maybe` value because it will be `Nothing` in the case of a rule 
representing a Free Move.

If, after processing some input, any of the machine's current states (*or any
states we can reach via a free move*) are in its list of "accept" states, the
machine has accepted the input.

> accepts :: NFA -> [Char] -> Bool
> accepts nfa = accepted . foldl' process nfa
>
>   where
>     accepted :: NFA -> Bool
>     accepted nfa = any (`elem` acceptStates nfa) (currentStates nfa ++ freeStates nfa)

Processing a single character means finding any followable rules for the 
given character and the current machine state, and following them.

> process :: NFA -> Char -> NFA
> process nfa c = case findRules c nfa of
>     -- Invalid input should cause the NFA to go into a failed state. 
>     -- We can do that easily, just remove any acceptStates.
>     [] -> nfa { acceptStates = [] }
>     rs -> nfa { currentStates = followRules rs }
>
> findRules :: Char -> NFA -> [Rule]
> findRules c nfa = filter (ruleApplies c nfa) $ rules nfa

A rule applies if

1. The read character is a valid input character for the rule, and
2. That rule applies to an available state

> ruleApplies :: Char -> NFA -> Rule -> Bool
> ruleApplies c nfa r =
>     maybe False (c ==) (inputChar r) &&
>     fromState r `elem` availableStates nfa

An "available" state is one which we're currently in, or can reach via 
Free Moves.

> availableStates :: NFA -> [SID]
> availableStates nfa = currentStates nfa ++ freeStates nfa

The process of finding free states (those reachable via Free Moves) gets 
a bit hairy. We need to start from our current state(s) and follow any 
Free Moves *recursively*. This ensures that Free Moves which lead to 
other Free Moves are correctly accounted for.

> freeStates :: NFA -> [SID]
> freeStates nfa = go [] (currentStates nfa)
>
>   where
>     go acc [] = acc
>     go acc ss =
>         let ss' = followRules $ freeMoves nfa ss
>         in go (acc ++ ss') ss'

Free Moves from a given set of states are rules for those states which 
have no input character.

> freeMoves :: NFA -> [SID] -> [Rule]
> freeMoves nfa ss = filter (\r ->
>     (fromState r `elem` ss) && (isNothing $ inputChar r)) $ rules nfa

Of course, the states that result from following rules are simply the 
concatenation of those rules' next states.

> followRules :: [Rule] -> [SID]
> followRules = concatMap nextStates

Now we can model an NFA and see if it accepts a string or not. You could 
test this in `ghci` by defining an NFA in state 1 with an accept state 2 
and a single rule that moves the machine from 1 to 2 if the character 
"a" is read.

```
ghci> let nfa = NFA [Rule 1 (Just 'a') [2]] [1] [2]
ghci> nfa `accepts` "a"
True
ghci> nfa `accepts` "b"
False
```

Pretty cool.

What we need to do now is construct an NFA whose rules for moving from 
state to state are derived from the nature of the pattern it represents. 
Only if the NFA we construct moves to an accept state for a given string 
of input does it mean the string matches that pattern.

> matches :: String -> Pattern -> Bool
> matches s = (`accepts` s) . toNFA

We'll define `toNFA` later, but if you've loaded this file, you can play 
with it in `ghci` now:

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

Before I show `toNFA`, we need to talk about mutability.

<h3>A Bit About Mutable State</h3>

Since `Pattern` is a recursive data type, we're going to have to 
recursively create and combine NFAs. For example, in a `Concat` pattern, 
we'll need to turn both sub-patterns into NFAs then combine those in 
some way. In the Ruby implementation, Mr. Stuart used `Object.new` to 
ensure unique state identifiers between all the NFAs he has to create. 
We can't do that in Haskell. There's no global object able to provide 
some guaranteed-unique value.

What we're going to do to get around this is conceptually simple, but 
appears complicated because it makes use of monads. All we're doing is 
defining a list of identifiers at the beginning of our program and 
drawing from that list whenever we need a new identifier. Because we 
can't maintain that as a variable we constantly update every time we 
pull an identifier out, we'll use the `State` monad to mimic mutable 
state through our computations.

<div class="well">
I apologize for the naming confusion here. This `State` type is from the 
Haskell library and has nothing to with the states of our NFAs.
</div>

First, we take the parameterized `State s a` type, and fix the `s` 
variable as a list of (potential) identifiers:

> type SIDPool a = State [SID] a

This makes it simple to create a `nextId` action which requests the next 
identifier from this list as well as updates the computation's state, 
removing it as a future option before presenting that next identifier as 
its result.

> nextId :: SIDPool SID
> nextId = do
>     (x:xs) <- get
>     put xs
>     return x

This function can be called from within any other function in the 
`SIDPool` monad. Each time called, it will read the current state (via 
`get`), assign the first identifier to `x` and the rest of the list to 
`xs`, set the current state to that remaining list (via `put`) and 
finally return the drawn identifier to the caller.

<h2>Pattern &rArr; NFA</h2>

Assuming we have some function `buildNFA` which handles the actual 
conversion from `Pattern` to `NFA` but is in the `SIDPool` monad, we can 
evaluate that action, supplying an infinite list as the potential 
identifiers, and end up with an NFA with unique identifiers.

> toNFA :: Pattern -> NFA
> toNFA p = evalState (buildNFA p) [1..]

As mentioned, our conversion function, lives in the `SIDPool` monad, 
allowing it to call `nextId` at will. This gives it the following type 
signature:

> buildNFA :: Pattern -> SIDPool NFA

Every pattern is going to need at least one state identifier, so we'll 
pull that out first, then begin a case analysis on the type of pattern 
we're dealing with:

> buildNFA p = do
>     s1 <- nextId
>
>     case p of

The empty pattern results in a predictably simple machine. It has one 
state which is also an accept state. It has no rules. If it gets any 
characters, they'll be considered invalid and put the machine into a 
failed state. Giving it no characters is the only way it can remain in 
an accept state.

>         Empty -> return $ NFA [] [s1] [s1]

Also simple is the literal character pattern. It has two states and a 
rule between them. It moves from the first state to the second only if 
it reads that character. Since the second state is the only accept 
state, it will only accept that character.

>         Literal c -> do
>             s2 <- nextId
>
>             return $ NFA [Rule s1 (Just c) [s2]] [s1] [s2]

We can model a concatenated pattern by first turning each sub-pattern 
into their own NFAs, and then connecting the accept state of the first 
to the start state of the second via a Free Move. This means that as the 
combined NFA is reading input, it will only accept that input if it 
moves through the first NFAs states into what used to be its accept 
state, hop over to the second NFA, then move into its accept state. 
Conceptually, this is exactly how a concatenated pattern should match.

*Note that `freeMoveTo` will be shown after.*

>         Concat p1 p2 -> do
>             nfa1 <- buildNFA p1
>             nfa2 <- buildNFA p2
>
>             let freeMoves = map (freeMoveTo nfa2) $ acceptStates nfa1
>
>             return $ NFA
>                 (rules nfa1 ++ freeMoves ++ rules nfa2)
>                 (currentStates nfa1)
>                 (acceptStates nfa2)

We can implement choice by creating a new starting state, and connecting 
it to both sub-patterns' NFAs via Free Moves. Now the machine will jump 
into both NFAs at once, and the composed machine will accept the input 
if either of the paths leads to an accept state.

>         Choose p1 p2 -> do
>             s2 <- nextId
>             nfa1 <- buildNFA p1
>             nfa2 <- buildNFA p2
>
>             let freeMoves =
>                     [ freeMoveTo nfa1 s2
>                     , freeMoveTo nfa2 s2
>                     ]
>
>             return $ NFA
>                 (freeMoves ++ rules nfa1 ++ rules nfa2) [s2]
>                 (acceptStates nfa1 ++ acceptStates nfa2)
>

A repeated pattern is probably hardest to wrap your head around. We need 
to first convert the sub-pattern to an NFA, then we'll connect up a new 
start state via a Free Move (to match 0 occurrences), then we'll connect 
the accept state back to the start state (to match repetitions of the 
pattern).

>         Repeat p -> do
>             s2 <- nextId
>             nfa <- buildNFA p
>
>             let initMove = freeMoveTo nfa s2
>                 freeMoves = map (freeMoveTo nfa) $ acceptStates nfa
>
>             return $ NFA
>                 (initMove : rules nfa ++ freeMoves) [s2]
>                 (s2: acceptStates nfa)
>

And finally, our little helper which connects some state up to an NFA via 
a Free Move.

>   where
>     freeMoveTo :: NFA -> SID -> Rule
>     freeMoveTo nfa s = Rule s Nothing (currentStates nfa)

<h2>That's It</h2>

I want to give a big thanks to Tom Stuart for writing Understanding 
Computation. That book has opened my eyes in so many ways. I understand 
why he chose Ruby as the book's implementation language, but I find 
Haskell to be better-suited to these sorts of modeling tasks. Hopefully 
he doesn't mind me exploring that by rewriting some of his examples.
