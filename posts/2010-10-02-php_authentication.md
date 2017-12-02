---
title: PHP Authentication
tags: self
---

Recently I had the opportunity to write some php pages (some mine,
some others) that required simple authentication. Nothing worthy of
social security or credit card numbers; but just enough to keep
something from being public.

In my case it was an admin script for the comments left on this
site. I could view all of the most recent comments and click a link
to mark any as spam. Doing this would remove all comments made with
that IP address as well as blacklist it from any future additions.

Anyway, the authentication part was simple. It only took a little
googling, so I thought I'd share the method I landed on.

## Authenticate

First, I wrote a small php script to hold the authentication logic.
It would have one method, `authenticate()` that would accept an
array of `(user => password)` values. If it fails, the page can't
go any further. I just keeps prompting for user/pass until there's
a valid login or the user hits cancel. At which time you'll see a
*Not authorized* page.

It serves its purpose easily, with the added bonus that it's hidden
behind a simple `authenticate()` call that I can update as needed.

```php 
<?php

function do_auth() {
    // prompt for password
    header('WWW-Authenticate: Basic realm="pbrisbin dot com"');
    header('HTTP/1.0 401 Unauthorized');

    // if user cancels
    header('Content-type: text/plain');
    echo 'Not authorized.';
    exit;
}

function authenticate($_valid_users) {
    // credentials not known
    if (!isset($_SERVER['PHP_AUTH_USER']))
        do_auth();

    $user = $_SERVER['PHP_AUTH_USER'];
    $pass = $_SERVER['PHP_AUTH_PW'];

    // user not known
    if (!isset($_valid_users[$user]))
        do_auth();

    // bad password
    if ($_valid_users[$user] != $pass)
        do_auth();
}

?>
```

Usage is fairly simple; on any page that needs authentication, use
the following:

```php 
<?php require_once('path/to/authentication.php');

$valid_users = array( 'user1' => 'password1'
                    , 'user2' => 'password2'
                    );

authenticate($valid_users);

// rest of page logic...

?>
```

Is it awesome? Is it safe? Is it secure? Probably not. But it
serves the purpose I need. And, is it easy? Yes.

<div class="well">
The PHP `header()` function has to be the absolute first thing to
generate any output from your page. This means you can't embed this
authentication logic in a page with any printed HTML (static or
coded) ahead of it.
</div>
