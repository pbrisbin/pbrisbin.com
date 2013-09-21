---
layout: post
title: "Beware Never Expectations"
tags:
  - mocha
  - rails
  - ruby
---

Mocha expectations are incredibly useful for ruby unit testing. You can 
stub out all kinds of functionality you depend on, specify exactly what 
values those dependencies return, and validate that the object under 
test behaves exactly as you want it to, right down to the methods it 
does or doesn't call.

Unfortunately, I've bumped up against one glaring case where this can 
get you into trouble. To make matters worse, the symptom of this 
situation is that your tests just always pass.

## Expects never

Take a simple class like this:

```ruby 
class Foo
  def a_method
    another_method

  rescue => ex
    logger.error("#{ex}")
  end

  # ...

end
```

Say we want to update it so that `another_method` is only called if some 
condition is met.

Let's play hardcore TDD here; we'll write *and run* the test first:

```ruby 
class FooTest < Test::Unit::TestCase
  def test_a_method
    foo = Foo.new

    # condition met
    foo.stubs(:some_condition?).returns(true)

    # should call it
    foo.expects(:another_method).once
    foo.a_method

    # condition not met
    foo.stubs(:some_condition?).returns(false)

    # should not call it
    foo.expects(:another_method).never
    foo.a_method
  end
end
```

Simple, easy to follow -- you should absolutely fail with "Unexpected 
invocation" on that second call due to the `never` expectation you've 
set. There's no reason to run this test now right? You *know* it's going 
to fail, right?

You run the test anyway, and it... Passes. Um, *wat*?

I know what you might be saying here, it's as simple as a single postfix 
`unless some_condition?`. So why am I insisting on figuring this out, 
wasting time just to see this test fail before I implement?

Well, in actuality I didn't do things in this order. I wrote the 
implementation, then ran the test, saw it pass and moved on. It was only 
later that I regressed, broke the implementation and didn't find out for 
a good while because the test never started failing. 

Luckily, it hadn't gone to production, but this scenario makes a strong 
case for writing *and running* your tests before your implementations -- 
it's the only chance you have to ensure your test actually covers what 
it should.

## Anti-rescue

Let me save you the frustration of debugging this. What's happening here 
is that when the method gets (incorrectly) called, Mocha raises an 
`ExpectationError` which is (by design) promptly `rescue`d and logged.

<div class="note">
I'd personally like to see Mocha not use this approach; rather count the 
number of calls and compare that number against what was expected later 
outside of your (possibly rescued) logic. This is how not-called-enough 
is implemented, why not let called-too-much be handled the same way?

</div>

There are a couple of ways we can work around this limitation though. 
One approach could be to re-raise the error when testing:

```ruby 
  # ...

rescue => ex
  logger.error(...)

  raise ex if Rails.env.test?
end
```

That's only moderately smelly and might suit you in most cases. In my 
case, I couldn't do this because swallowing all errors was by-design and 
(of course) backed up with test coverage, so those would start failing 
if I re-raise in that environment.

That and I hate modifying implementation code specifically to support 
some testing-related concern.

Another option might be to specifically handle the Mocha exception:

```ruby 
  # ...

rescue Mocha::ExpectationException => ex
  raise ex
rescue => ex
  logger.error(...)
end
```

That exception class is not in scope when you're running in production, 
so that wouldn't be fun. And I'd be **very** against requiring the Mocha 
gem in non-test environments.

## Rewrite never

Anyway, here's the solution we ended up with: redefine the method to 
increment a class-level counter, then assert that it was never called by 
checking that counter afterwards.

```ruby 
class FooTest < Test::Unit::TestCase
  def test_a_method
    foo = Foo.new
    foo.stubs(:some_condition?).returns(false)

    assert_not_called(foo, :another_method) do
      foo.a_method
    end
  end

  private

  # Note: not thread-safe
  def assert_not_called(obj, method, &block)
    # set a class level counter
    @@counter = 0

    # redefine the method so, if called, it increments that counter
    obj.instance_eval %{
      def #{method}(*args)
        #{self.class}.instance_eval "@@counter += 1"
      end
    }

    # run your code
    yield

    # see if it was ever called
    assert_equal 0, @@counter, "#{obj}.#{method}: unexpected invocation."
  end
end
```

Now, do yourself a favor and run this test *before* you write the 
implementation. It's the only way to be sure the test works and 
regressions will be caught down the line.
