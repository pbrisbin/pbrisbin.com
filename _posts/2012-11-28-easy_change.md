---
layout: post
title: "Easy Change"
tags:
  - ruby
  - refactoring
---

> for each desired change, make the change easy (warning: this may be 
> hard), then make the easy change
>
> &mdash; Kent Beck, [September 25, 2012][tweet]

[tweet]: https://twitter.com/KentBeck/status/250733358307500032

Here's some code from our main application helper. It provides a small 
method for redirecting the user based on a `goto` parameter. It uses two 
helpers itself to append google analytics parameters to the url before 
redirecting.

Originally, it was uncommented. I've added a few here to highlight what 
goes through my head when first reading it.

```ruby 
def get_ga_params
  # Nice use of Explaining Temporary Variable to avoid a Magic Number 
  # situation, but this list of keys seems generally useful and would 
  # rarely change. Why not a real constant?
  analytics_keys = %w(utm_campaign utm_source utm_medium)

  # minor, but return is unneeded
  return params.reject { |k,v| !analytics_keys.include? k }
end

def append_ga_params(url)
  # warning: shadowing outer variable, url
  returning(url) do |url|
    # Why treat p like an array when it's a hash, should use k,v here. 
    # Also, I prefer map vs collect.
    query_string = get_ga_params.collect {|p| "#{p[0]}=#{p[1]}"}

    # Should use string interpolation. Also, I'd prefer "if present?" 
    # to "unless blank?". Finally, I'd place the check ahead of building 
    # the query string both to be (slightly) more efficient and to get 
    # it higher up in the method so I don't need to think about it while 
    # deciphering the string building here.
    url << "?" << query_string.join("&") unless query_string.blank?
  end
end

def redirect_to_latest_or_goto
  goto = params[:goto]

  unless goto =~ %r[^/]
    goto = latest_events_path
  end

  redirect_to(append_ga_params(goto))
end
```

So the methods are somewhat smelly, but not enough to warrant 
refactoring when you don't need to make a change in this area.

Fortunately, Business has decided that they would like to append the 
`BuyId` parameter to the redirect url in much the same was the analytics 
parameters are currently.

Our first instinct might be to just add the param inside the 
`append_ga_params` method. This would be incorrect; since `BuyId` is not 
a google analytics parameter, the name of the method would be 
misleading.

Alternatively, we could just plop the param onto the end of the url 
directly in `redirect_to_latest_or_goto`. Adding some string building 
into that method might be considered mixing layers of abstraction. It 
also does nothing to explain what we're doing the way `append_ga_params` 
does.

## Make the Change Easy

It'd be really nice if we had a generic `append_params` helper available 
to add our `BuyId`. This is basically what `append_ga_params` is doing, 
except that it's over specified. 

Let's tease that logic out into a separate method and call it from our 
original. At the same time we can clean up some of the smells we noticed 
earlier.

```ruby 
def append_params(url, new_params)
  # Quick guard statement
  return url if new_params.empty?

  # Treats the hash like a hash
  query_str = new_params.map { |k,v| "#{k}=#{v}" }.join('&')

  # This switch could be done a number of ways, I'm not yet sure which I 
  # prefer.
  if url.include?('?')
    "#{url}&#{query_str}"
  else
    "#{url}?#{queyr_str}"
  end
end

# Promoted to a constant
ANALYTICS_KEYS = %w[ utm_campaign utm_source utm_medium ]

def get_ga_params
  # Now one line
  params.reject { |k,v| !ANALYTICS_KEYS.include? k }
end

def append_ga_params(url)
  # Now one line
  append_params(url, get_ga_params)
end

def redirect_to_latest_or_goto
  goto = params[:goto]

  unless goto =~ %r[^/]
    goto = latest_events_path
  end

  redirect_to(append_ga_params(goto))
end
```

Notice that we keep the original methods' interfaces exactly as they 
were. This should allow any existing tests to pass without modification 
and give us confidence that we've gotten it right.

In my case, `append_ga_params` was not marked `private`. If it were I'd 
probably do all this a bit differently. For now, we decide to play it 
safe and leave the class interface alone.

With tests passing, we commit our code and shift gears from Refactor to 
Feature.

## Make the Easy Change

```diff 
+ BUY_ID = 123

 def redirect_to_latest_or_goto
   goto = params[:goto]

   unless goto =~ %r[^/]
     goto = latest_events_path
   end

-  redirect_to(append_ga_params(goto))
+  url = append_ga_params(goto)
+  redirect_to(append_params(url, 'BuyId' => BUY_ID))
 end
```

## Conclusion

This was definitely a simple example, but it's nice to see how this 
two-step process works on something realistic. It's not difficult to 
extrapolate this up to something larger.
