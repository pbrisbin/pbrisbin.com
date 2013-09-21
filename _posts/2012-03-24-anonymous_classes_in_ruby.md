---
layout: post
title: "Anonymous Classes In Ruby"
tags:
  - ruby
---

Often times, I find myself wanting something anonymous. This occurs 
quite frequently in code when you need to define, pass or call some 
functionality which is usually very short and only useful in this 
moment. Many languages provide anonymous functions (usually called 
`lambdas`) for this sort of thing: haskell has `\x y -> x + y` and ruby 
has `lambda {|x,y| x + y}`, `Proc.new` and the new `->(x,y)` syntax 
which I'm actually not very fond of.

Sometimes, in ruby, I find myself wanting an anonymous class for much 
the same reasons. At first, this seemed like a silly thing to do, so I 
didn't expect it to be clean or easy -- but in fact, it is.  Ruby itself 
uses anonymous classes for all sorts of things, and the syntax we'll use 
to do it is almost comically obvious.

## Testing

Sometimes if you're writing a test for a module, you need to `include` 
or `extend` it into something to accurately test it. Here's one approach 
to doing that:

```ruby 
# assume MyModule is defined as a module, which you want to test

class ModuleTest < Test::Unit::TestCase
  def test_the_thing
    klass = MyClass.new

    # assert something about klass now that it's included your module
  end

  private

  class MyClass
    include MyModule

    # ...

  end
end
```

This is fairly contrived, but I think we all agree that sometimes you 
need a new class to test something (like modules). Putting in some 
private subclass for the purposes of testing seems fairly appropriate, 
albeit pretty smelly.

Let's see how an anonymous class can help:

```ruby 
class ModuleTest < Test::Unit::TestCase
  def test_the_thing
    klass = Class.new do
      include MyModule

      # ...

    end.new

    # assert something about klass
  end
end
```

Not only is this a bit shorter, but I'd say it's clearer too now that 
the object under test is made more prominent.

## Rake tasks

I like to write rake tasks to do useful things. Sometimes one of those 
tasks wants to move files around. `FileUtils` is great for this, and 
it's best used when mixed into a class.

I won't bore you with the non-anonymous version, so here's the one using 
`Class.new`, hopefully you can imagine it with more boilerplate:

```ruby 
require 'fileutils'

desc "do the damn thing"
task :run do
  Class.new do
    include FileUtils

    def run!
      mv this, that

      cp here, there

      rm the_thing
    end
  end.new.run!
end
```

So short!

This really speaks to ruby's flexibility when it comes to "everything is an 
object" and hopefully illustrates that if you understand the benefits of 
anonymous functions, why not start thinking about how to use anonymous 
classes too?
