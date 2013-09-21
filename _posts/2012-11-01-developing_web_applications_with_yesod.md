---
layout: post
title: "Developing Web Applications with Yesod"
tags:
  - haskell
  - yesod
  - published
---

<div class="note">
The following was written for [issue 7][issue] of Web & PHP magazine. 
Please, if you enjoy this article (or my articles in general), take the 
two minutes to register there and download the full PDF to show your 
support.
</div>

[issue]: http://webandphp.com/user/register?destination=issue-7

## Why Haskell?

There's much more to Haskell than just the buzz-words like laziness and 
parallelism -- which are completely deserved, by the way. Having pure 
computations defined as side-effect-free morphisms that take and return 
immutable datatypes allows the compiler to do *amazing* optimizations. 
This frees you to write elegant, readable code but get near-C 
performance at the same time.

Runtime errors. `grep`ping through source to find some method you just 
rewrote to ensure it's not incorrectly called somewhere. Wondering if 
that expression represents a String or a Boolean. Wondering how that 
template behaves when `user` is `nil`. Unit tests. Hitting deploy and 
frantically browsing the site to make sure things still work. These are 
the hazards of a dynamic language. These are things that all go away 
when you use a language like Haskell.

It's been my experience that when developing in Haskell: if it compiles, 
it works. I'd say, conservatively, that 93% of every bug I've ever 
written in Haskell has been caught immediately by the compiler. That's a 
testament to both the compiler and the number of bugs I'm able to 
produce in Haskell code. It's amazingly freeing to gain such a level of 
confidence in the correctness of your code simply by seeing its 
successful compilation.

My hope for this article is to illustrate this experience by building 
out a simple site in the Haskell web framework Yesod. Yesod is just one 
of many web frameworks in Haskell, but it's the one I'm most comfortable 
with. I encourage you to check it out at [yesodweb.com][], there are 
many features and considerations that I won't be touching on here.

[yesodweb.com]: http://yesodweb.com

## Yesod Development

In order to develop a Yesod site, you'll need the Glasglow Haskell 
Compiler, along with some additional build tools. These can all be 
installed by setting up the Haskell Platform. There are installers for 
Windows, OSX, and most Linux distributions have it in their 
repositories.

Once the Haskell Platform is setup, type:

```
$ cabal update
$ cabal install yesod-platform
```

This one time installation of the framework can take while.

## The Lemonstand

The blog example feels a bit overdone, doesn't it? In stead, let's build 
a lemonade stand (which I'll refer to as "The Lemonstand" from now on). 
We won't get too crazy with features, we just want a few pages and some 
database interaction so you can see how this framework can be used.

Much of the site will be provided by the code-generating tool called the 
Yesod Scaffold.

## Yesod Init

The Yesod scaffolding tool will build out a sample site showing some of 
the more common and useful patterns used in Yesod sites. It will build 
you a simple "hello world" site with important features like 
persistence, authentication and static file serving already coded out. 
You can then edit and extend this site to quickly build out features.

<div class="note">
It's important to note that this is not *the* way to structure a Yesod 
application, is just *one* way to do it. That said, this organisational 
structure has been refined over a long period of time and comes with 
many benefits.
</div>

To start our project, we do the following:

```
$ yesod init
```

We'll answer a couple of questions about ourselves and our project. I'm 
calling it "lemonstand" and choosing the sqlite database type since it 
does not require a separate server.

The first thing we have to do is pull in any additional dependencies 
(like the driver for the type of database we chose to use).

```
$ cd lemonstand
$ cabal install
```

In order for authentication via Google to work (a feature we'll use down 
the line), we need to make one small change to `config/settings.yml`. 
Please update the development block like so:

```yaml 
Development:
  <<: *defaults
  approot: "http://localhost:3000"
```

With that bit of housekeeping out of the way, go ahead and fire up the 
development server:

```
$ yesod devel
```

You should see lots of output about compilation, database migrations, 
etc. Most importantly is "Devel application launched: 
http://localhost:3000". Go ahead and checkout the sample site by 
visiting that URL in your browser.

<div class="image">
![default screenshot](/img/lemonstand/default.png)
</div>

Now we're ready to hack!

## Models

In a "production" lemonade stand app, we might get a little more complex 
with the data modeling, but to keep this demo simple, I'm going to model 
the system simply as well.

The scaffold already comes with the concept of a `User` and 
authentication, so we'll keep that as-is. The second most important 
concept will be `Order`s which our users can create through a typical 
check-out flow.

`Order`s will have many `Lemonade`s which have `size`, `price`, and 
`quantity`.

Open up `Model.hs`. This is where we'll place our core data type 
definitions. You should notice a line about `persistFile`. What this 
does is parse the text file "config/models" and generate some Haskell 
datatypes for us. This line also adds the required boilerplate to 
persist these types to the database and well as the initial migration 
code. This is where your `User` model comes from.

We'll get to this file in a second, but first we're going to define some 
data types that *won't* be persisted.

Go ahead and add the following after the `import` lines but before the 
`share` line:

```haskell 
type Price = Double
type Qty   = Int
```

What these are are type *aliases*. They just allow you to refer to one 
type as another (`[Char]` is aliased to `String` in the standard 
Prelude, for instance).

If and when we later make functions that deal with `Lemonade`s, we'll 
see type signatures like this:

```haskell 
-- | Calculate the total price of multiple lemonades
totalPrice :: [Lemonade] -> Price
totalPrice = ...
```

And not like this:

```haskell 
-- | Calculate the total price of multiple lemonades
totalPrice :: [Lemonade] -> Double
totalPrice = ...
```

Which is not as descriptive. It's a little thing, but it goes a long 
way.

We're also going to create an additional data type that won't (itself) 
be stored as a database record.

```haskell 
data Size = Small
          | Medium
          | Large
          deriving (Show, Read, Eq, Ord, Enum, Bounded)
```

You might be familiar with this concept as an `enum`. In Haskell, the 
concept of enumeration types are just a degenerative form of algebraic 
data types where the constructors take no arguments.

Don't worry about the `deriving` line. That just tells Haskell to go 
ahead and use sane defaults when performing common operations with this 
type like converting it to string or comparing two values for equality. 
With this `deriving` in place, Haskell knows that `Small` can be shown 
as "Small" and that `Medium == Medium`.

Even though we don't want to store `Size`s directly in the database as 
records, we do plan to have fields of other records be of the type 
`Size`. To allow this, we just have to ask Yesod to generate some 
boilerplate on this type:

```haskell 
derivePersistField "Size"
```

Easy.

When you hit save on this file, you should see in your terminal still 
running `yesod devel` that it's recompiled your sources and restarted 
your development server. The important thing is that it does this 
successfully each time you make a change. When you introduce a bug, 
you'll get a compiler error directing you to the problem. This immediate 
and accurate feedback is important to the development process as we'll 
see later on.

Next, we'll go ahead and add some database models. Open up 
`config/models`.

You'll see some models are already present, we'll just add more to the 
bottom of the file:

```
Order
    user UserId

Lemonade
    order OrderId Maybe
    size  Size
    price Price
    qty   Qty
```

This is exactly as if you had handwritten the Haskell data types:

```haskell 
data Order = Order
    { orderUser :: UserId
    }

data Lemonade = Lemonade
    { lemonadeOrder :: Maybe OrderId
    , lemonadeSize  :: Size
    , lemonadePrice :: Price
    , lemonadeQty   :: Qty
    }
```

In addition to the above declarations, Yesod will add all of the 
boilerplate needed for values of these types to be (de)serialized and 
persisted to or restored from the database.

Again, save the file and make sure it compiles.

<div class="note">
Notice that I used the `Maybe` type on `lemonadeOrder`. In Haskell, the 
this type is defined as:

```haskell 
data Maybe a = Just a | Nothing
```

This allows you to have a function which can return some `a` or 
`Nothing` at all. This is how Haskell can maintain type safety even when 
you need the concept of an optional parameter or return value.

I'm assuming here that we might want to describe `Lemonade`s that aren't 
yet associated with an `Order`. We'll see if that turns out to be the 
case.
</div>

## Route Handling

Before we start making further changes, let me provide some context on 
how the current homepage is rendered. We'll be mimicking this pattern 
for our other pages.

Every URL that your app responds to is listed in `config/routes`, so go 
ahead and open that file.

You'll see some scaffold-provided routes already. `/static` and `/auth` 
use a concept called Subsites to provide additional functionality to 
your app (namely static file serving and user authentication). We'll not 
go into this any further as it can get hairy quickly and for the 
purposes of this article, we can treat these as black boxes.

The rest of the entries are normal routes. For these, you provide:

1. The relative URL you answer to (we'll get to variable pieces later)
2. The data type of the route (again, more later)
3. The supported methods (GET, POST, etc)

Let's look at `HomeR`.

In your `Foundation.hs` file there's another line similar to the 
`persistFile` line in `Model.hs`. It works much the same way in that it 
will parse this flat file (`config/routes`) and generate some Haskell 
code for us.

When the parser comes across this `HomeR` line, it's going to do a 
number of things. Conceptually, it's something like the following:

1. `HomeR` is made a valid constructor for values of type `Route` which 
   is used by the framework to route requests to your handler functions.
2. The functions in charge of rendering and parsing URLs can now 
   translate to and from this `HomeR` type.

In order to accomplish this, two functions need to be in scope: 
`getHomeR` and `postHomeR`. This is because we've specified GET and POST 
as supported methods.

So, whenever a GET request comes in for "/", Yesod will now translate 
that URL into the data type `HomeR` and know to call `getHomeR` which is 
a function that returns an HTML response (`RepHtml`).

<div class="note">
If you were to define a route like "/users/#UserId UsersR GET", then 
your required function `getUsersR` would have the type `UserId -> 
RepHtml`. Since your URL has a variable in it, that piece will match as 
a `UserId` and it will be given as the first argument to your handler 
function -- all in an entirely type safe way.
</div>

Let's add a route for buying some lemonade:

```
/checkout CheckoutR GET POST
```

While we're here, remove the POST from `HomeR` since we'll no longer be 
using that.

When you save this file you should see some problems in your compiler 
window:

```
[7 of 7] Compiling Application      ( Application.hs, 
dist/build/Application.o )

Application.hs:30:1: Not in scope: `getCheckoutR'

Application.hs:30:1: Not in scope: `postCheckoutR'
```

Well, look at that. We've introduced a bug, and it was caught 
immediately.

Since the app now needs to answer requests for "/checkout" by calling 
your handler functions, they need to be there or you'd have runtime 
errors. There is very little potential for runtime errors in Haskell, 
and this is just our first example of why: the compiler catches us ahead 
of time.

So let's fix it. The following steps might feel a bit tedious, and in 
Yesod version 1.1 there is a tool to do them for you, however I think 
that doing things like this manually at least once is useful.

Add the following around line 36 of lemonstand.cabal:

```
Handler.Checkout
```

This tells the build system to include this new source file we'll 
create.

Add the following around line  26 of Application.hs:

```haskell 
import Handler.Checkout
```

This imports that module (still not written) into the scope where these 
functions are needed.

Finally, create the file `Handler/Checkout.hs`:

```haskell 
module Handler.Checkout where

import Import

getCheckoutR :: Handler RepHtml
getCheckoutR = undefined

postCheckoutR :: Handler RepHtml
postCheckoutR = undefined
```

We've really just traded one runtime error for another as visiting that 
page will result in the app calling `undefined` which will fail. 
However, we've made the compiler happy and can move onto other things 
and come back to these later.

## Templates

Let's open up `Handler/Home.hs` and see how our current home page is 
rendered.

We're going to strip out just about everything here. Similar code will 
be added later in other handlers, and I'd like you to see those concepts 
then rather than now.

Rewrite the file so it looks like this:

```haskell 
-- leave everything up to and including the import line as-is.

getHomeR :: Handler RepHtml
getHomeR = do
    -- Use the default overall layout, you'll amost always do this.
    defaultLayout $ do

        -- The page title.
        setTitle "Lemonade Stand"

        -- The template to render.
        $(widgetFile "homepage")
```

You may notice, you've triggered another compiler error, quite a few 
actually: `not in scope: aDomId`.

Our templates reference a variable which we've just removed. Please, 
take a moment to appreciate type-safe templates. No runtime error, no 
silent nil-handling, we get an up-front compiler error indicating 
exactly where the problem is. How cool is that?

In the process of fixing this, I'll also try to provide a little more 
context.

`$(widgetFile "homepage")` is a very useful function. What it does is 
look in your `templates` directory for any HTML, CSS and Javascript 
templates for your "homepage". These templates will be combined into a 
`Widget`. `Widget`s can be nested and combined quite naturally 
throughout your application. In the end, they will all be rolled up into 
one final `Widget` and served as a single response. All style sheets and 
scripts will be concatenated, minified (when configured to do so) and 
ordered correctly -- all without you having to think about it.

For us, this means `templates/homepage.{hamlet,lucius,julius}` are being 
found and compiled.

Julius is Javascript templating, it's essentially a straight passthrough 
except with variable interpolation. You can go ahead and remove it now, 
we won't use it on this page.

```
$ rm templates/homepage.julius
```

Lucius is a superset of CSS. It was designed to allow existing CSS to be 
pasted directly in and have it still compile and work. On top of this, 
it allows for variable interpolation and some Less-like extensions like 
nesting and mixins. Open up the template and remove the style block 
referencing `aDomId`.

Hamlet is the most complex of Yesod's templaters. Open up the template 
and fill it with the following content:

```
<h1>_{MsgHello}

<p>
  Click 
  <a href=@{CheckoutR}>here
  \ to buy some Lemonade!
```

We're going to leave `_{MsgHello}` in place. The `_{ }` interpolation 
will check your messages file for translations and show different 
content based on the user's preferred language.

`@{ }` is a Route interpolation. As you might've guessed, it's used to 
show internal links in a type safe way. Now that we've removed the 
`aDomId` references things are compiling, but it's important to realize 
that had we added this link to `CheckoutR` in here *before* actually 
adding that route to our app, we'd get a similar compiler error. No more 
dead links in your application, any URLs that don't resolve will 
immediately show up as compiler errors.

<div class="note">
If we had a route as mentioned before for users ("/users/#UserId") we'd 
have to use something like `@{UsersR aUserId}` and the compiler would 
infer and enforce that `aUserId` is, in fact, a `UserId`.
</div>

There is a lot of functionality in Hamlet templates, some of which we'll 
get to when we build out our next page. What you can do right now is 
refresh your browser and see your changes.

<div class="image">
![homepage screenshot](/img/lemonstand/homepage.png)
</div>

## Forms

Let's head back to `Handler/Checkout.hs`. We're going to add a very 
simple form where the user can pick the size of their lemonade and 
checkout.

First we'll declare a form:

```haskell 
lemonadeForm :: Form Lemonade
lemonadeForm = renderTable $ Lemonade
    <$> pure Nothing
    <*> areq (selectField optionsEnum) "Size" Nothing
    <*> pure 0.0
    <*> areq intField "Quantity" Nothing
```

There's a few things going on here worth looking at. First of all, each 
line represents a record of the `Lemonade` data type. When shown, this 
form will have fields according to what's listed and map those values 
back to a value of type `Lemonade` when the form is processed. The lines 
that use `pure` provide values when processed, but don't actually show 
any fields.

<div class="note">
Where going to cheat here and completely ignore `Price`. Dealing with 
dependent fields (setting price based on size, for example) can get 
tricky, so we're just going to set the price server-side after the size 
and quantity have been submitted.
</div>

Before we can test out this form, there's one thing we need to change 
about our `Foundation.hs`. We're going to use the function 
`requireAuthId` to force users to authenticate before checking out. This 
function also gives us the Id of the current user.

To allow this, we've got to change the module exports of `Foundation.hs` 
like so:

```haskell 
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , requireAuthId -- <- add this
    , module Settings
    , module Model
    ) where

```

With that in place, we can sketch out the Handler now:

```haskell 
getCheckoutR :: Handler RepHtml
getCheckoutR = do
    -- force authentication and tell us who they are
    uid <- requireAuthId

    -- run the defined form. give us a result, the html and an encoding 
    -- type
    ((res,form), enctype) <- runFormPost $ lemonadeForm

    case res of
        -- if a form was posted we get a Lemonade
        FormSuccess l -> do
            -- process it and give us the order id
            oid <- processOrder uid l

            -- TODO: redirect to Thank You page here
            return ()

        -- in all other cases just "fall through"
        _ -> return ()

    -- and display the page
    defaultLayout $ do
        setTitle "Checkout"
        $(widgetFile "checkout")

postCheckoutR :: Handler RepHtml
postCheckoutR = getCheckoutR

processOrder :: UserId -> Lemonade -> Handler OrderId
processOrder = undefined
```

When `requireAuthId` is encountered for an unauthenticated user, they 
will be redirected to login. The scaffold site uses the `GoogleEmail` 
plugin which allows users to login using their gmail accounts via Open 
Id. This authentication system can of course be changed, extended or 
removed, but we're going to just use it as is.

We're also using a common idiom here: the same Handler handles both GET 
and POST requests. In the case of a GET, the form result (`res`) will be 
`FormMissing`, that `case` statement will fall through and the `form` 
will be displayed. In the case of a POST, the form result will be 
`FormSuccess`, we'll execute `processOrder` (which we've left 
`undefined` for now) and redirect to a "Thank You" page.

Additionally, if there were errors in the parameters, the results would 
be `FormErrors` which is handled the same way as a GET (fall through to 
displaying the form) except this time, the form's HTML will include 
those errors so they're visible to the user to correct and resubmit.

Upon saving this, we should have another compiler error. We've told 
yesod to look for "checkout" templates, but there are none. So let's 
create just "templates/checkout.hamlet":

```
<h1>Checkout
<p>What size lemonade would you like?
<form enctype="#{enctype}" method="post">
  <table>
    ^{form}
    <tr>
      <td>&nbsp;
      <td>
        <button type="submit">Checkout
```

Simple variable interpolation is done via `#{ }`, while embedding one 
template (like `form`) into another is done via `^{ }`.

<div class="image">
![form screenshot](/img/lemonstand/form.png)
</div>

Now that we've got the form showing, we can replace our `undefined` 
business logic with some actual updates:

```haskell 
-- | Take a constructed Lemonade and store it as part of a new order in 
--   the database, return the id of the created order.
processOrder :: UserId -> Lemonade -> Handler OrderId
processOrder uid l = runDB $ do
    oid <- insert $ Order uid
    _   <- insert $ l { lemonadeOrder = Just oid
                      , lemonadePrice = priceForSize $ lemonadeSize l
                      }

    return oid

    where
        priceForSize :: Size -> Price
        priceForSize Small  = 0.99
        priceForSize Medium = 1.99
        priceForSize Large  = 2.99
```

Make sure that compiles, then add in the actual `redirect`:

```haskell 
getCheckoutR :: Handler RepHtml
getCheckoutR = do
    uid <- requireAuthId

    ((res,form), enctype) <- runFormPost $ lemonadeForm

    case res of
        FormSuccess l -> do
            oid <- processOrder uid l

            -- redirect to a "Thank You" page which takes an order id as 
            -- a parameter.
            redirect $ ThankYouR oid

        _ -> return ()

    defaultLayout $ do
        setTitle "Checkout"
        $(widgetFile "checkout")
```

Hopefully, you've noticed the compiler error this introduces. Can you 
guess how to fix it?

We've told our Application to redirect to `ThankYouR` but that route 
does not exist. Again, no runtime error, just a clear compiler error.

So, follow the advice of the compiler and add the route declaration to 
`config/routes`:

```
/thank_you/#OrderId ThankYouR GET
```

Again we get the expected compiler error that `getThankYouR` is not in 
scope.

In the interest of time and variety, we'll not create an entirely 
different module, or template for the Thank You page, We'll inline 
everything right here in `Handler/Checkout.hs`:

```haskell 
getThankYouR :: OrderId -> Handler RepHtml
getThankYouR oid = defaultLayout $ do
    setTitle "Thanks!"

    [whamlet|
        <h1>Thank You!
        <p>Your order is ##{toPathPiece oid}
        |]
```

<div class="image">
![thank you screenshot](/img/lemonstand/thank_you.png)
</div>

## Conclusion

Obviously, The Lemonstand is quite lacking. The user never gets to see 
price, there's no concept of buying multiple Lemonades of varying Sizes, 
and the overall UI/UX is pretty terrible.

These are all things that can be fixed, but this article is already 
getting quite long, so I'll have to leave them for another time. 
Hopefully you've seen a good enough mix of theory and practice to agree 
that there are benefits to working on web applications (or any software) 
in a purely functional language like Haskell.
