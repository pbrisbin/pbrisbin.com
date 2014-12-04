---
title: Implicit Scope
tags: ruby, rails, work
---

No one can deny that rails likes to do things for you. The term 
"auto-magically" comes to mind. This can be a blessing and a curse.

For the most part, rails tries to give you "outs" -- a few hoops here 
and there that, if jumped though, will let you do things in different or 
more manual ways. Sometimes though, it doesn't.

## Find In Batches

One of the many <abbr title="object relational mapping">ORM</abbr> 
helpers provided by rails is `find_in_batches`. It will repeatedly query 
the database with a limit and offset, handing you chunks of records to 
work through in sequence. Perfect for processing a very large result set 
in constant memory.

```ruby 
Order.find_in_batches(:batch_size => 10) do |orders|
  orders.length # => 10

  orders.each do |order|
    
    # yay order!

  end
end
```

The problem is that any conditions you add to `find_in_batches` are 
inherited by any and all sql performed within its block. This is called 
"implicit scope" and there's no way around it.

Why is this an issue? I'm glad you asked, here's a real life example:

```ruby 
#
# SELECT * from orders
# WHERE orders.status = 'pending'
# LIMIT 0, 10;
#
# adjusting LIMIT each time round
#
Order.find_in_batches(:batch_size => 10,
                      :conditions => ['status = ?', 'pending']) do |orders|

  orders.each do |order|
    #
    # UPDATE orders SET orders.status = 'timing_out'
    # WHERE orders.id     = ?
    #   AND orders.status = 'pending'; <-- oh-hey implicit scope
    #
    order.update_attribute(:status, 'timing_out')

    #
    # some long-running logic to actually "time out" the order...
    #

    #
    # UPDATE orders SET orders.status = 'timed_out'
    # WHERE orders.id     = ?
    #   AND orders.status = 'pending';
    #
    order.update_attribute(:status, 'timed_out')
  end
end
```

Do you see the problem? The second update fails because it can't find 
the order due to the implicit scope. The first update was only 
successful due to coincidence.

## Workaround

I would love to find a simple `remove_implicit_scope` macro that can get 
around this issue, but it's just not there.

I even went so far as to put the update logic in a `Proc` or `lambda` 
hoping to bring in a binding without the implicit scope -- no joy.

I had to resort to simply not using `find_in_batches`.

At the time, I just rewrote that piece of the code to use a `while true` 
loop. Thinking about it now, I realize I could've factored it out into 
my own `find_in_batches`; also, I could put it in a module so you can 
`extend` it in your model to have the better (IMO) behavior...

```ruby 
module FindNoScope

  def find_in_batches(options)
    limit = options.delete(:batch_size)
    options.merge!(:limit => limit)

    offset = 0

    while true
      chunk = all(options.merge(:offset => offset))
  
      break if chunk.empty?
  
      yield chunk
    end
  
    offset += limit
  end

end

class Order < ActiveRecord::Base
  extend FindNoScope

  # ...

end
```

*Note that the above was written blind, is completely untested, and will 
likely not work*
