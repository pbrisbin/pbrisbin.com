---
layout: post
title: "Table links"
tags:
  - html
  - javascript
  - website
---

Often you might want to present a table of items, each of which links to 
its own page. Typically you might add an additional cell with a link to 
go to the item-specific page.

Wouldn't it be better if the entire row was itself clickable? Well, I 
did the googling, and here's one easy way I've found to accomplish that.

You'll need a little jQuery:

```javascript 
$(function() {
  $('tbody.link tr').click(function() {
    window.location = $(this).find('a').attr('href');
  }).hover(function() {
    $(this).toggleClass('pointer');
  });
});
```

Of course, you can choose to put this only on pages that need it, but 
it's not very heavy and if it's on your site-wide template, you can 
quickly apply this method to any table you want by just adding a class 
to the `tbody` tag (all of your tables do have `thead` and `tbody` tags, 
*right*?)

For that hover callback to have the desired effect and let your users 
know they should click on the row, you'll need a little bit of css as 
well:

```css 
.pointer { cursor: pointer; }
```

You could put this css change right in the javascript, but I find this 
`pointer` class comes in handy throughout my site anyway.

Finally, for any tables which you want to behave this way, just use 
markup like the following:

```html 
<table>
  <thead>
    <tr>
      <th>Name</th>
      <th>Description</th>
    </tr>
  </thead>

  <tbody class="link"><!-- 1. add the class -->
    <tr>
      <td>
        <a href="/items/1"></a><!-- 2. add the link(s) -->
        Item_1
      </td>
      <td>
        The first item
      </td>
    </tr>
    <tr>
      <td>
        <a href="/items/2"></a>
        Item_2
      </td>
      <td>
        The second item
      </td>
    </tr>

    <!-- ... -->

  </tbody>
</table>
```

Notice the content of the "Name" field is outside of the link tag and 
the link itself has no content. This ensures no actual link will be 
visible to confuse users, all they have to do is click anywhere on the 
row.

For a real example, checkout the [archives][] page.

[archives]: /archives/
