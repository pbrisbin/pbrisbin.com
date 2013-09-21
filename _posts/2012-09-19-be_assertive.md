---
layout: post
title: "Be Assertive with Sane Exception Handling"
tags:
  - ruby
  - rails
---

I'm a big fan of Avdi Grimm's thoughts about writing confident ruby. I 
think it's important to not clutter things with a bunch of nil-checks or 
exception handling. When you're focused in at the method level you 
should trust that your objects are valid and the methods you're calling 
behave.

<div class=note>
In this post I use the term "assertive" to mean much the same thing as 
Avdi's "confident". I think I like this better because the definition of 
assertive contains both "confident" and "forceful".

Your code needs to be both confident that the objects it deals with 
behave and forceful that the objects dealing with it behave.
</div>

One way to give yourself this freedom is to lean on sane exception 
handling at a higher level of abstraction. This is a concept I'm just 
now formulating myself, but it's really starting to pay dividends.

I believe you should have a single place at each major layer of 
abstraction where exceptions are handled. You should also err on the 
side of less error handling and let any exceptions propagate up as high 
in the abstraction stack as possible until you absolutely *have to* do 
something about them to prevent a poor user experience.

If you rescue exceptions within the internals of your application, 
you're hiding valuable information about why and how something failed. 
Over handling also leads to cluttered, complicated code.

## Commandline Apps

One example where I use this approach is in commandline applications. I 
force myself to have a single rescue statement in the entire app:

```ruby 
module ImperativeShell
  class Main
    class << self
      def run!(argv)

        # program logic using all kinds of internal classes which may 
        # raise exceptions or call library code that may itself raise 
        # exceptions

      rescue => ex
        if debug?
          # collect what you need as a developer
        end

        $stderr.puts("#{ex}")
        exit 1
      end
    end
  end
end
```

I'll regularly `git grep rescue` and if I can't 100% justify anything I 
see, I take it out.

## Rails Controllers

Another place where I've really seen benefit to this approach is in a 
set of controllers I had to write for a new API layer we're building at 
work.

I knew that no matter what happened within our controller logic, I 
wanted to give the client a valid JSON response with a proper status 
code. Supporting this sort of catch all behavior was pretty easy with a 
`rescue_from` in the base controller:

```ruby 
module Api
  class Base < ActionController::Base

    rescue_from Exception do |ex|
      logger.error("api error: #{ex}")

      error = {
        :code        => 500,
        :name        => "#{ex.class}",
        :description => "Something's gone horribly wrong!"
      }

      # a simple render :json helper
      respond_error(500, error)
    end

  end
end
```

With this one simple catch-all, I can now be sure that no matter what 
happens in my controllers, things will behave gracefully.

I realize now that this isn't a design that only makes sense in this 
particular scenario, there are tons of places through all the apps I 
work on where, when I'm deep in some class or method, I don't want to 
deal with and/or hide the exceptions that might be thrown by the various 
libraries and methods I'm calling.

Exception handling is a feature and it should be treated as such. This 
means it needs to be well thought out and the logic needs to exist in 
the right place and do the right thing. Moreover, that should not be my 
concern when I'm working on some small `send_forgot_password_email` 
method on the `User` model. If the mail client throws an exception, I'm 
not the guy that should be handling that. Whoever called me probably 
wants to know about it. And if you follow the line of callers up the 
stack there should be someone somewhere who can turn that into a pretty 
message to tell the user who originally asked to have their password 
reset that something's gone wrong. If any one of these callers gets 
greedy, the whole thing turns into a kludge.

```ruby 
def send_forgot_password_email
  if mailer = UserMailer.new(self)
    unless mailer.deliver_forgot_password_email rescue nil
      return false
    end
  end

  true
end
```

This is obfuscated code. Don't use `nil` or `false` as a valid return 
value to hide what really happened and signify generic failure. You're 
destroying valuable information about said failure.

Whether the `mailer` raises an exception or not is that object's 
concern. How that exception is conveyed to the end-user is your caller's 
concern. When you have a chance, at any layer of abstraction, to reduce 
your own number of concerns, do it.

```ruby 
def send_forgot_password_email
  UserMailer.new(self).deliver_forgot_password_email # assertive!
end
```

OK, end rant. Onto more uses of this pattern...

## Cleaner Routes

With a similar rescue for `ActionController::UnknownAction` we can 
*implicitly* handle the case of an API client calling a method we don't 
support and return the proper 501 - NotImplemented.

```ruby 
rescue_from ActionController::UnknownAction do |ex|
  error = {
    :code        => 501,
    :name        => "NotImplemented"
    :description => "#{ex}"
  }

  respond_error(501, error)
end
```

We even get a free description from the error. Printing the exception 
shows something like "No action for foo. Available: bar, baz." Which is 
exactly the behavior the HTTP spec dictates. These are the things rails 
does well. Follow the conventions, use the out of the box features to 
write less code yourself.

With this in place, you can take a routes file like this:

```ruby 
namespace(:api) do |api|
  api.resource :user, :conditions => [:index, :show] do |user|
    user.resource :cart, :conditions => [:create, :update]
  end
end
```

And strip out the conditions:

```ruby 
namespace(:api) do |api|
  api.resource :user do |user|
    user.resource :cart
  end
end
```

This might seem like a small matter of aesthetics (and even if it was, I 
still like it), but it's also more Agile. We know any undefined methods 
will return the proper response. As requirements inevitably change, we 
only have to make the single change of adding or removing methods; we 
don't then also have to go update the routes file. Win.

## Cleaner Actions

We can take this further still. How many times have you come across an 
action like this:

```ruby 
def show
  if params[:id].blank?
    # return some specific error response
  end

  unless m = Model.find_by_id(params[:id])
    # return some other specific error response
  end

  # actual logic

end
```

By explicitly adding `ActiveRecord::RecordNotFound` to our list of 
rescues we can remove all that cruft.

```ruby 
rescue_from ActiveRecord::RecordNotFound do |ex|
  error = {
    :code        => 404,
    :name        => "NotFound"
    :description => "#{ex}"
  }

  respond_error(404, error)
end
```

Again, we get a free description. We can now clean up the action to 
something much more assertive and simple like:

```ruby 
def show
  m = Model.find(params[:id])

  # actual logic

end
```

And both invalid states lead to the correct error descriptions of "Can't 
find Model without an ID" or "Can't find Model with ID=42" respectively. 
Thank you again, Mr Rails.

## Cleaner Everything

Once you get used to this method of exception handling and assertive 
code, it's easy to take this even further and define your own custom 
exception-rescue_from-raise scenarios for when your controllers get into 
various (exceptional) states where they can't and shouldn't continue.

No need to `and return` or `return render` or wrap everything in 
`if/unless` etc. When shit goes wrong, just `raise` the appropriate 
exception. All you have to do is trust (or dictate) that the level of 
abstraction(s) above you are written to do the Right Thing, which is a 
useful quality even if you're not following this pattern.
