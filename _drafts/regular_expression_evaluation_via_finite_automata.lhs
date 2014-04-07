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

We're going to model a simple subset of regular expressions. 
Specifically, we'll be able to check for the following patterns:

> data Pattern =
>       Empty                   -- ""
>     | Literal Char            -- "a"
>     | Concat Pattern Pattern  -- "ab"
>     | Choose Pattern Pattern  -- "a|b"
>     | Repeat Pattern          -- "a*"

We're going to check for matches against these patterns by converting 
them into [Deterministic Finite Automata][dfa] or DFAs, then letting the 
DFAs operate on the input strings and give us a useful answer.

[dfa]: http://en.wikipedia.org/wiki/Deterministic_finite_automaton

Since this is a recursive data type, we're going to have to recursively 
create and combine DFAs. For example, in a `Concat` pattern, we'll need 
to turn both sub-patterns into DFAs then combine those in some way. In 
the Ruby implementation, Mr. Stuart used `Object.new` to ensure unique 
state identifiers between all the DFAs he has to create. We can't do 
that in Haskell. There's no global state able to provide some 
guaranteed-unique value.

To get around this, we're going to use a monad which encapsulates an 
infinite list of available identifiers. We can then ask for an 
identifier whenever we need one, and ensure they're always unique within 
that computation.

We'll use normal integers for the state identifiers:

> type DFAState = Int

And we'll use the polymorphic `State s a` type, but fix the `s` variable 
as our list of potential IDs. I apologize for any naming confusion here. 
This `State` type is from the Haskell library and has nothing to with 
the states of our DFAs.

> type DSI a = State [DFAState] a
>
> runDSI :: DSI a -> a
> runDSI f = evalState f [1..]

We can define a `nextId` action which requests the next identifier from 
this list as well as updates the computation's state removing it as a 
future option.

> nextId :: DSI DFAState
> nextId = do
>     (x:xs) <- get
>     put xs
>     return x

So long as our DFA creating functions live in this monad and use 
`nextId`, we can guarantee the state identifiers will always be unique.

DFAs are a simple machine. They have some states and some rules. They 
read characters in and move from state to state according to those 
rules. Some states are special, they're known as "accept" states. What 
we're going to do is construct DFAs which have the logic of how the 
patterns match particular strings encoded in their rules such that 
moving into an accept state means the pattern matched the string.

This process of encoding logic in a set of mechanical instructions is 
very much the same thing we do every day when we write computer 
programs. I find it fascinating to see this process at work with only a 
single, thin layer of abstraction between the language and the machine 
rather than the thousands of layers we encounter every day.

> matches :: Pattern -> String -> Bool
> matches p = accepts $ runDSI $ toDFA p

We can add a test for it as `main`:

> main :: IO ()
> main = do
>     -- An AST for the pattern /ab|cd*/:
>     let p = Choose
>             (Concat (Literal 'a') (Literal 'b'))
>             (Concat (Literal 'c') (Repeat (Literal 'd')))
>
>     print $ matches p "xyz"
>     -- -- => False
>
>     print $ matches p "cddd"
>     -- => True

To make this work, we'll need

1. Some representation of a DFA
2. Some function `accepts :: DFA -> [Char] -> Bool`
3. Some function `toDFA :: Pattern -> DSI DFA`

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
>     , inputChars :: [Char]
>     , nextStates :: [DFAState]
>     } deriving Show

The reason `inputChars` and `nextStates` are lists is because we need to 
use this deterministic machine to model a non-deterministic one. It's 
possible (and required, in fact) to have a rule like *If in State 1 and 
an "a" is read, go to State 2 or State 3*. We can model that by having 
both 2 and 3 in `nextStates` for that rule. It's also possible (and also 
required) to have such a thing as a "Free Move". This means that the 
machine can change states without reading any input. This is useful if 
there is a state reachable via a Free Move which has a normal rule for 
the character about to be read. The machine can "jump" to that state, 
then follow that rule. These are modelled here by an empty list in the 
`inputChars` field.

<div class="well">
I may refactor this later to use `Maybe` or perhaps a separate data type 
and `DFA` field for the Free Moves.
</div>

If, after processing some input, any of the machine's current states are 
in its list of "accept" states, the machine has accepted the input.

> accepts :: DFA -> [Char] -> Bool
> accepts dfa = accepted . foldl' process dfa
>
> accepted :: DFA -> Bool
> accepted dfa = any (`elem` acceptStates dfa) (currentStates dfa)

Processing is done by finding any followable rules for the given 
character and the current machine, and following them.

> process :: DFA -> Char -> DFA
> process dfa c = case findRules c dfa of
>     -- invalid input should cause the DFA to go into a failed state. 
>     -- we can hack that for now by removing the acceptStates
>     [] -> dfa { acceptStates = [] }
>     rs -> dfa { currentStates = followRules rs }
>
> findRules :: Char -> DFA -> [Rule]
> findRules c dfa = filter (ruleApplies c dfa) $ rules dfa

A rule applies if

1. The read character is a valid input character for the rule, and
2. That rule leads directly out from our state, or
3. We can satisfy option 2 by taking a Free Move

> ruleApplies :: Char -> DFA -> Rule -> Bool
> ruleApplies c dfa r = c `elem` inputChars r &&
>     fromState r `elem` availableStates dfa
>
> availableStates :: DFA -> [DFAState]
> availableStates dfa = currentStates dfa ++ freeStates dfa

The process of finding free states (those reachable via Free Moves) gets 
a bit hairy. We need to start from our current state(s) and follow any 
Free Move rules *recursively*. To ensure that Free Moves which lead to 
other Free Moves are correctly accounted for.

> freeStates :: DFA -> [DFAState]
> freeStates dfa = go [] (currentStates dfa)
>
>   where
>     go acc [] = acc
>     go acc ss =
>         let ss' = followRules $ freeMoves dfa ss
>         in go (acc ++ ss') ss'
>
> freeMoves :: DFA -> [DFAState] -> [Rule]
> freeMoves dfa ss = filter ((`elem` ss) . fromState)
>                  $ filter (null . inputChars) $ rules dfa

Of course, the states that result from following more than one rule are 
simply the concatenation of those rules' next states.

> followRules :: [Rule] -> [DFAState]
> followRules = concatMap nextStates

At this point, we can model a DFA and see if it accepts a string or not. 
All that's left is to convert patterns into appropriate DFAs. To do 
this, we'll write the function `toDFA` which is in the `DSI` monad, 
allowing it to call `nextId` at will.

This gives it the following type signature:

> toDFA :: Pattern -> DSI DFA

Every pattern is going to need at least one state identifier, so we'll 
pull that out first, then begin a case analysis on the type of pattern 
we're dealing with:

> toDFA p = do
>     s1 <- nextId
>
>     case p of

The empty pattern results in a predictably simple machine. It has one 
state which is also an accept state. It has no rules. Take a minute to 
work through how this will accept an empty string.

>         Empty -> return $ DFA [] [s1] [s1]

Also simple is the literal character pattern. It has two states and a 
rule between them. It moves from the first state to the second only if 
it reads that character. Since the second state is the only accept 
state, it will only accept that character.

>         Literal c -> do
>             s2 <- nextId
>
>             return $ DFA [Rule s1 [c] [s2]] [s1] [s2]

We can model a concatenated pattern by first turning each sub-pattern 
into their own DFAs, and then connecting the accept state of the first 
to the start state of the second via a Free Move. This means that as the 
combined DFA is reading input, it will only accept that input if it 
moves through the first DFAs states into what used to be its accept 
state, hop over to the second DFA, then move into its accept state. 
Conceptually, this is exactly how a concatenated pattern should match.

*Note that `freeMoveTo` will be shown after*.

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
>             let initMove = Rule s2 [] (currentStates dfa)
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
>     freeMoveTo dfa s = Rule s [] (currentStates dfa)

And that's it, DFA-powered Regular Expressions in Haskell. Combine this 
is some Parsec code to turn "/foo|bar/" into a `Pattern` and you might 
have something half-way usable!
