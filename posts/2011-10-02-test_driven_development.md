---
title: Test Driven Development
tags: ruby
---

With my recent job shift, I've found myself in a much more sophisticated 
environment than I'm used to with respect to Software Engineering.

At my last position, there wasn't much existing work in the X++ realm; 
We were breaking new ground, no one cared about elegance; if you got the 
thing working -- more power to you.

Here, it's slightly different.

People here are working in a sane, documented, open-source world; and 
they're good. Everyone is acutely aware of what's good design and what's 
not. There's a focus on elegant code, industry standards, solid 
<abbr title="object oriented programming">OOP</abbr> principles, and 
most importantly, we practice Test Driven Development.

I'm completely new to this method for development, and I gotta say, it's 
quite nice.

Now, I'm not going to say that this is the be-all-end-all of development 
styles (I'm a functional, strictly-typed, compiler-checked code guy at 
heart), but I do find it quite interesting -- and effective.

So why not do a write-up on it?

## Test Framework

The prerequisite for doing anything in TDD is a good test framework. 
Luckily, ruby is pretty strong in this area. The way it works is the 
following:

You subclass `Test::Unit` and define methods that start with `test_` 
where you execute system logic and make assertions about certain 
results; and then you run that class.

Ruby looks for those methods named as `test_whatever` and runs them "as 
tests". Running a method as a test means that errors and failures (any 
of your `assert` methods returning false) will be logged and displayed 
at the end as part of the "test report".

All of these test classes can be run automatically by a build-bot and 
(depending on your test coverage) you get good visibility into what's 
working and what's not.

This is super convenient and empowering in its own right. In a dynamic 
language like ruby, tests are the only way you have any level of 
confidence that your most recent code change doesn't blow up in 
production.

So now that you've got this ability to write and run tests against your 
code base, here's a wacky idea, write the tests *first*.

## Test Driven

It's amazing what this approach does to the design process.

I've always been the type that just starts coding. I'm completely 
comfortable throwing out 6 hours worth of code and starting over. I know 
my "first draft" isn't going to be right (though it will be useful). I 
whole-heartedly believe in refactorings, etc. But most importantly, I 
need to code to sketch things out. It's how I've always worked.

TDD is sort of the same thing. You do a "rough sketch" of the 
functionality you'll add simply by writing tests that enforce that 
functionality.

You think of this opaque object -- a black box. You don't know *how* it 
does what it does, but you're trying to test it doing it.

This automatically gives you an end-user perspective. You now focus 
solely on the interface, the input and the output.

This is a wise position to design from.

You also tend to design small self-contained pieces of functionality. 
Methods that don't care about state, return the same output for a given 
input, and generally do one simple thing. Of course, you do this because 
these are the easiest kind of methods to test.

So, out of sheer laziness, you design a cohesive, easy to use, and 
completely simple *interface*, an API.

Now you just have to "plumb it up". Hack until the tests pass, and 
you're done. That might be an over-simplification, but it's not off by 
much...

Come to think of it, this is exactly the type of design Haskell favors. 
With gratuitous use of `undefined`, the super-high-level logic of a 
Haskell program can be written out with named functions to "do the heavy 
lifting". If you make these functions simple enough and give them 
descriptive enough names, they practically write themselves.

So that's TDD (at least my take on it). So far, I like it.
