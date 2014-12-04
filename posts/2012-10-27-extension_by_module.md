---
title: Extension by Module
tags: ruby, metaprogramming
---

Ruby's open classes are great for adding behavior to existing objects. 
Though it's a language feature, there to be used, I'd argue that the 
majority of times it is used, Open classes weren't the most appropriate 
tool.

First of all, you may be setting yourself (and other developers) up for 
confusion. Not knowing where methods come from or why a method behaves 
oddly can be a problem. In the majority of cases, I find you've got an 
instance of some object, and you just want to add behavior to *it*.

In these cases, opening up a class and adding behavior to **all** 
instances --past, present, and future-- is a bit over-kill. It'd be more 
appropriate to add behavior to just that instance.

## Open classes

If you're intention is to make `String`s greppable, opening up the 
`String` class might look appealing

<div class="well">
I'm aware that `Enumerable` already provides this functionality. It's 
just an example.
</div>

```ruby 
class String
  def grep(regex)
    lines = self.split("\n")
    lines.select { |s| s =~ regex }
  end
end

title_info = `vobcopy -I '#{device}' 2>&1`

@title    = title_info.grep(/Most chapters/).first.split(' ')[5]
@dvd_name = title_info.grep(/Name of the dvd/).first.split(' ')[5]
```

Works great.

## Modules

The same thing can be accomplished with a module.

```ruby 
module Grep
  def grep(regex)
    lines = self.split("\n")
    lines.select { |s| s =~ regex }
  end
end

title_info = `vobcopy -I '#{device}' 2>&1`
title_info.extend(Grep)

@title    = title_info.grep(/Most chapters/).first.split(' ')[5]
@dvd_name = title_info.grep(/Name of the dvd/).first.split(' ')[5]
```

The main benefits here are that a) the addition of behavior is made 
explicit and b) you only change the one instance you're working with 
rather than affecting every `String` throughout the entire system.

One interesting implication of learning this is realizing that using 
`extend` inside a class definition, though conceptually different, is 
technically identical to the above.

```ruby 
class MyClass
  extend MyModule

end
```

De-sugared, this is actually `MyClass.extend(MyModule)` which is 
analogous to `my_string.extend(Grep)`. The former adds methods from 
`MyModule` onto `MyClass` just as the latter adds `Grep`s methods onto 
`my_string` 

At its core, ruby is a very simple language. It takes core 
Object-oriented concepts (like "extending" some object) and abides by 
them at each layer of the abstraction stack.

This allows a little bit of knowledge about the internals of the 
language to pay substantial dividends in actual implementations.
