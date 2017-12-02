---
title: Console TDD with String IO
tags: ruby
---

If you write console based applications in ruby, chances are you're 
going to want to get some test coverage on that eventually. `StringIO` 
is a great class to use when you want to assert that your application 
outputs the correct stuff to the screen.

We can modify the global variable `$stdout` to be an instance of 
`StringIO` for the duration of our tests. Any method that outputs text 
on `stdout` (like `puts` and `print`) will be sending their text to this 
object. After we're done, we can ask it what it's got and make 
assertions on it.

Here's an `rspec` example:

```ruby 
require 'stringio'

describe StringIO do
  before do
    $stdout = StringIO.new
  end

  after do
    # always clean up after yourself!
    $stdout = STDOUT
  end

  it "should help capture standard output" do
    puts "foo"
    puts "bar"

    $stdout.string.should == "foo\nbar\n"
  end
end
```

Not a bad bit of TDD if I don't say so!

Similar tricks could be used with `$stderr` or `$stdin` to get solid 
end-to-end test coverage on a wide variety of console-based 
applications.
