---
title: Live Search (part 2)
tags: javascript, website
---

In my last [post][] I went over setting up sphinx full-text search using 
an xml data source from a yesod application as well as hooking into 
sphinx to return search results for a given query as a JSON data feed.

[post]: /posts/live_search

In this (shorter) post, I'll go over the front-end javascript that I 
used to implement a fairly simple search-as-you-type interface.

## Object oriented

Now, I could have easily defined some simple functions in the global 
namespace to execute the search, display the results, then attach an 
event handler to the changes to the input box, but I'd rather not.

Javascript can be used fairly effectively in an object oriented way. No, 
I'm not doing any inheritance or method overloads, but I do want to try 
and group all my logic in an instance of some object. This will let me 
store some values in instance variables (properties) for use between 
methods as well as give me a namespace for all my stuff.

Here's the structure:

```javascript 
var Search = {
    execute: function(qstring) {
        // actually execute the search and call display as the success 
        // callback
    },

    display: function(results) {
        // update the page with the contents of the search results.
    },

    attach: function() {
        // attach a listener for changes to the input element and fire 
        // off the search when appropriate.
    }
};
```

Our feed is accessed at `/search/j/query-string` and returns something 
like this:

```json 
[
  {
    "slug":    "some_post",
    "title":   "Some post",
    "excerpt": "... some excerpt with matches in it ..."
  },
  {
    "slug":   "other_post",
    "title":  "Other title",
    "excerpt":"... other excerpt with matches ..."
  },
  ...
]
```

Given that, our execute and display functions should look like this:

```javascript 
    execute: function(qstring) {
        var search = this;
        var url    = "/search/j/" + encodeURIComponent(qstring);

        $.getJSON(url, function(data) {
            search.display(data);
        });
    },

    display: function(results) {
        var html = "";

        $.each(results, function(id, result) {
            html += '<div class="result">'
                  + '<h3><a href="/posts/' + result['slug'] + '/">' + result['title'] + "</a></h3>"
                  + '<div class="result-excerpt">' + result['excerpt'] + '</div>'
                  + '</div>';
        });

        // assume this property exists for now
        this.results.html(html);
    },
```

Our attach method will handle a few things:

1. Store selectors for the input element and the results container as 
   properties on our object.

2. Attach a listener to the input element that fires every time a 
   character is entered.

3. Check that the entered search term is non-empty, big enough, and 
   has actually changed -- to prevent a "needless" search.

```javascript 
    attach: function() {
        this.search  = $('#search');
        this.results = $('#results');

        var search = this;

        this.search.keyup(function() {
            var $this = $(this);

            var newVal = $this.val();
            var oldVal = $this.data('old-value');

            if (newVal.length >= 3 && newVal != oldVal) {
                search.execute(newVal);
            }

            $this.data('old-value', newVal);
        });
    }
```

I use jQuery's `data` function to store the input's current value 
between each event to see if it's changed since last time.

Note that we also have to store a reference to `this` outside of the 
`keyup` callback, since calling `this` inside that closure means 
something else (the element itself).

With all that in place, a search page that uses this object would look 
something like this:

```html 
<input id="search">

<div id="results"></div>

<script>
    $(function() {
        Search.attach();
    });
</script>
```

That's it, simple and effective. Go ahead, [try it out][archive].

[archive]: /archives
