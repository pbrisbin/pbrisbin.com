---
title: Testing POROs In Rails
tags: ruby
---

A lot of smarter people than I have come up with the idea that moving 
your business logic out of framework based classes and into Plain Old 
Ruby Objects (POROs) is a Good Thing.

Being less tied to your framework makes upgrades easier. It also means 
you can test these objects without loading rails. That's really the only 
way to get super fast unit tests. It's also the only way to get true 
and isolated unit tests.

The problem is that even our PORO code will have some implicit 
dependencies on parts of rails. Some of these are Good Things that we 
don't want to stamp out, others are just too annoying to work around in 
the PORO or its spec. This makes testing them in isolation difficult and 
awkward, which is a key indicator of a Bad Idea.

What we need is a compromising spec helper. One which brings in *just 
enough* rails so these niceties work as any rails developer might expect 
but not so much rails that it slows down the suite in any noticeable 
way. This lets our rails app be developed using common conventions and 
idioms but we also get a sick-fast Unit suite.

I'm figuring this out as I go, exploring through a toy application, but 
here's what I've got so far:

**spec/spec_helper.rb**

```ruby 
# It's super convenient to type 10.hours or t.to_s(:db), we want to be 
# able to use these things freely in our PORO code.
require 'active_support/core_ext'

# Unless you have concerns about making inter-class dependencies 
# explicit, there's really no good reason to add requires all over the 
# place.
require 'active_support/dependencies'

# Yes, this is a bit of duplication you have to manage between here and 
# your application config, but I think it's worth it.
%w( lib app/models ).each do |dir|
  $LOAD_PATH << dir
  ActiveSupport::Dependencies.autoload_paths << dir
end

# Without fail, a PORO is going to reference an AR-based class. Your 
# PORO spec can (and should) stub all the behavior of that class, but 
# simply loading it will reference this constant.
module ActiveRecord
  class Base
    # You should also make no-ops for any class-level macros you use. 
    # Again, we should be stubbing as needed, this just allows the class 
    # definition to be sourced without error.
    class << self
      def belongs_to(*); end
      def has_many(*)  ; end
      # ...
    end
  end
end
```
