---
title: Posts Database
tags: haskell, self
---


<div class="well">
This post is crazy out of date. If your interested in the updated ways 
in which I accomplish the same goals on yesod 0.9, feel free to checkout 
the site's [source][].
</div>

[source]: https://github.com/pbrisbin/devsite

For the longest time since my move to Yesod, I've had the listing of 
posts on this site embedded in its source. This is not only poor 
practice, but it made it kludgy to add a new post the site. I'd have to 
write the post, add the Post info to the source file, then recompile my 
development sources and deploy them to production.

This marriage of code and content made it impossible to write posts and 
develop anything new in the framework at the same time because both 
actions required a "push to PROD".

The actual content for each post is stored in a markdown file and parsed 
at run time, so I could at least edit content while the site is live. 
Ideally, I would've put the post meta-data (title, date published, rss 
description, and tags) as a header in this markdown file and have that 
parsed at runtime as well. This would've been difficult when it came to 
the rss description. Parsing multi-line tokens is no fun.

So given the requirement that post content and post meta-data would live 
separately I decided to give Persist a try. I could store post 
information in a small sqlite database and access it through some 
management pages to add, remove, and update post meta-data on my site.

This approach required me to work through three aspects of Yesod that 
aren't extremely well documented: Persistent, Forms, and Authentication. 
I figured I'd share what I did in this post and maybe others can 
benefit (or point out what I've done wrong).

I'm going to skip over any required extensions or imports just to keep 
the code presented here simple and somewhat readable.

If you're able to use Yesod in some way, you can probably decipher ghc 
errors enough to figure out what's needed.

## Persistent

First up was getting <abbr title="Create Update Delete">CUD</abbr> 
actions possible on `Post`s (the existing data type).

I had to make my site an instance of `YesodPersist` which will allow 
abstract data base actions to happen in the `GHandler` Monad:

```haskell 
-- I added this to Settings.hs:
dataBase :: String
dataBase = "posts.db3"

withConnectionPool :: MonadInvertIO m => (ConnectionPool -> m a) -> m a
withConnectionPool = withSqlitePool dataBase 10

-- And this to the main DevSite.hs file:
instance YesodPersist DevSite where
    type YesodDB DevSite = SqlPersist
    runDB db = fmap connPool getYesod >>= runSqlPool db
```

Next up, I had to use some template haskell to define the storable 
entries:

```haskell 
-- I elected to put this code right in Posts.hs which handles all the 
-- Post related stuff already:
share2 mkPersist (mkMigrate "migratePosts") [$persist|
SqlPost
    slug        String
    date        UTCTime Desc
    title       String
    descr       String
    UniqueSqlPost slug
SqlTag
    post SqlPostId Eq
    name String Asc
|]
```

This creates two tables. One to hold all the post information and a 
second to just hold the Post-to-Tag relationships.

We also have to add the migration function to the main method which will 
initialize these tables on first run:

```haskell 
-- This goes in my Controller.hs:
withServer :: (Application -> IO a) -> IO a
withServer f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts) p -- right here
    let h = DevSite p
    toWaiApp h >>= f
```

With all the boilerplate in place, we can write some functions to 
actually do the selects, inserts, and deletes (I cheat and actually do a 
delete-then-insert for any updates):

```haskell 
-- | The data type of a single post, this is what we actually want to work 
--   with in all our other code
data Post = Post
    { postSlug  :: String
    , postDate  :: UTCTime
    , postTitle :: String
    , postDescr :: String
    , postTags  :: [String]
    }

-- | Select n recent posts from the database and return them
selectPosts :: Int -> Handler [Post]
selectPosts n = mapM go =<< runDB (selectList [] [SqlPostDateDesc] n 0)

    where
        go :: (Key SqlPost, SqlPost) -> Handler Post
        go (sqlPostKey, sqlPost) = do
            -- tags for this post
            sqlTags <- runDB $ selectList [SqlTagPostEq sqlPostKey] [SqlTagNameAsc] 0 0
            return Post
                { postSlug  = sqlPostSlug  sqlPost
                , postDate  = sqlPostDate  sqlPost
                , postTitle = sqlPostTitle sqlPost
                , postDescr = sqlPostDescr sqlPost
                , postTags  = fmap (sqlTagName . snd) sqlTags
                }

-- | Insert a post into the database
insertPost :: Post -> Handler ()
insertPost post = do
    let sqlPost = SqlPost
            { sqlPostSlug  = postSlug post
            , sqlPostDate  = postDate post
            , sqlPostTitle = postTitle post
            , sqlPostDescr = postDescr post
            }

    -- insert the Post record
    sqlPostKey <- runDB $ insert sqlPost

    -- insert each tag record
    mapM_ (go sqlPostKey) $ postTags post

    where
        go :: SqlPostId -> String -> Handler SqlTagId
        go key tag = runDB (insert $ SqlTag key tag)

-- | Delete an existing post by slug
deletePost :: String -> Handler ()
deletePost slug = do
    sqlPost <- runDB $ getBy $ UniqueSqlPost slug
    case sqlPost of
        Just (sqlPostKey, _) -> do
            -- delete the post and the tags
            runDB $ deleteBy $ UniqueSqlPost slug
            runDB $ deleteWhere [SqlTagPostEq sqlPostKey]
        Nothing -> return ()
```

As an example of the simple case, selecting a post out of the data base 
and displaying it, here is my handler for a GET request on the post 
route plus some of the immediate support functions:

```haskell 
-- | This was already in place with the hardcoded Posts so I just put it 
--   in the Handler Monad and call from the database instead.
-- 
--   I know, it's silly to not make the slug part of a targetted select, 
--   but with such a small data base this is fine and much easier to 
--   code.
-- 
getPostBySlug :: String -> Handler [Post]
getPostBySlug slug = do
    allPosts <- selectPosts 0
    return $ filter ((== slug) . postSlug) allPosts

-- | Used with posts so that we have post-specific info within scope
--   while still abstracting the overall template/css
postLayout :: Post -> Handler RepHtml
postLayout post = do
    mmesg       <- getMessage
    (t, h)      <- breadcrumbs
    postContent <- loadPostContent post -- parses the markdown file

    pc <- widgetToPageContent $ do
        setTitle $ string $ "pbrisbin - " ++ postTitle post
        addCassius $(S.cassiusFile "root-css")
    hamletToRepHtml $(S.hamletFile "post-layout")

-- | Load a Post
getPostR :: String -> Handler RepHtml
getPostR slug = do
    posts <- getPostBySlug slug
    case posts of
        []       -> notFound
        (post:_) -> postLayout post
```

So now that that part's done, we need a page where we can edit and 
delete the existing posts. That will require a form.

## Forms

I probably have a bit more boilerplate here than I need, but oh well. I 
had a lot of this code already in 
[yesod-comments](https://github.com/pbrisbin/yesod-comments) where I 
need a bit more customization in the form so I reused it.

The first thing we need is an overall page which has an "Add new post" 
form at the top and a table of existing posts with links to edit and 
delete them:

```haskell 
-- | The overall template showing the input box and a list of existing
--   posts
managePostTemplate :: String -> Widget () -> Enctype -> Widget ()
managePostTemplate title form enctype = do
    posts <- liftHandler $ selectPosts 0
    [$hamlet|
    <div .post_input>
        <h3>#{string title}

        <form enctype=#{enctype} method="post"
            ^{form}

    <div .posts_existing>
        <h3>Existing posts:

        <table>
            <tr>
                <th>Title
                <th>Description
                <th>Edit
                <th>Delete

            $forall post <- posts
                <tr>
                    <td>
                        <a href=@{PostR $ postSlug post}> #{shortenShort $ postTitle post}
                    <td>#{shortenLong $ postDescr post}
                    <td>
                        <a href=@{EditPostR $ postSlug post}> edit
                    <td>
                        <a href=@{DelPostR $ postSlug post} delete
    |]

    where 
        shortenLong  = shorten 40 
        shortenShort = shorten 15 
        shorten n s  = if length s > n then take n s ++ "..." else s
```

Don't worry about EditPostR or DelPostR yet, we'll get to those.

Now we need to code the Form itself. The way I do it is I create a data 
type whose records represent the Form fields. Then, when I run the form, 
I'll use a function to convert that datatype into the thing I really 
want from the Form (a `Post`) with any required conversions or time 
stamping happening there.

To make things a little more flexible, I'm going to pass an initial 
argument to most of these functions. If that argument is `Just Post`, 
then I'm editing an existing post and I will pre-populate the "new" form 
with its information and update rather than insert on submit. If that 
first argument is `Nothing`, then it's a truly new `Post` and I'll 
continue as such.

This is why I pass `title` to the function above; it might say "Edit 
..." or "Add ..." accordingly.

```haskell 
-- the form data type
data PostForm = PostForm
    { formSlug  :: String
    , formTitle :: String
    , formTags  :: String
    , formDescr :: Textarea
    }

-- | Convert form input into a Post and update the db.
updatePostFromForm :: Maybe Post -> PostForm -> Handler ()
updatePostFromForm p pf = do
    postDate' <- if isJust p 
        -- preserve original publish date
        then return $ postDate $ fromJust p
        else liftIO getCurrentTime
    let post = Post
            { postSlug  = formSlug pf
            , postTitle = formTitle pf
            , postDescr = unTextarea $ formDescr pf
            , postDate  = postDate'
            , postTags  = parseTags $ formTags pf
            }
    if isJust p
        then do
            -- delete the original and insert a new version
            deletePost (postSlug post)
            insertPost post
            setMessage $ [$hamlet| %em post updated! |]
        else do
            insertPost post
            setMessage $ [$hamlet| %em post added! |]

    redirect RedirectTemporary ManagePostsR

-- | some minor changes to 
--   <https://github.com/fortytools/lounge/blob/master/Handler/Entry.hs#L57>
parseTags :: String -> [String]
parseTags [] = []
parseTags s  = let (l,s') = break (==',') $ dropWhile (==',') s
    in trim l : case s' of
        []      -> []
        (_:s'') -> parseTags s''

    where 
        trim  = trim' . trim' 
        trim' = reverse . dropWhile isSpace

-- | Display the new post form inself. If the first argument is Just,
--   then use that to prepopulate the form
postForm :: Maybe Post -> FormMonad (FormResult PostForm, Widget ())
postForm post = do
    (slug       , fiSlug       ) <- stringField   "post slug:"   $ fmap postSlug  post
    (title      , fiTitle      ) <- stringField   "title:"       $ fmap postTitle post
    (tags       , fiTags       ) <- stringField   "tags:"        $ fmap (formatTags . postTags) post
    (description, fiDescription) <- textareaField "description:" $ fmap (Textarea . postDescr)  post
    return (PostForm <$> slug <*> title <*> tags <*> description, [$hamlet|
        <table>
            ^{fieldRow fiSlug}
            ^{fieldRow fiTitle}
            ^{fieldRow fiTags}
            ^{fieldRow fiDescription}
            <tr>
                <td>
                    &nbsp;
                <td colspan="2">
                    <input type="submit" value=#{buttonText}>
        |])

    where
        fieldRow fi = [$hamlet|
            <tr>
                <th>
                    <label for=#{fiIdent fi}> #{fiLabel fi}
                    <div .tooltip> #{fiTooltip fi}
                <td>
                    ^{fiInput fi}
                <td>
                    $maybe error <- fiErrors fi
                        #{error}
                    $nothing
                        &nbsp;
            |]

        formatTags = intercalate ", "
        buttonText = string $ if isJust post then "Update post" else "Add post"

-- | Run the post form and insert or update based on the entered data
runPostForm :: Maybe Post -> Widget ()
runPostForm post = do
    ((res, form), enctype) <- liftHandler . runFormMonadPost $ postForm post
    case res of
        FormMissing    -> return ()
        FormFailure _  -> return ()
        FormSuccess pf -> liftHandler $ updatePostFromForm post pf

    managePostTemplate title form enctype

    where 
        title = if isJust post 
            then "Edit post:" 
            else "Add new post:"
```

Anyway, with all *that* boilerplate out of the way, we can define our 
routes. 

We need to add the following to our main `parseRoutes` function first:

```haskell 
-- | Define all of the routes and handlers
mkYesodData "DevSite" [$parseRoutes|
...

/manage                ManagePostsR GET POST
/manage/edit/#String   EditPostR    GET POST
/manage/delete/#String DelPostR     GET

/auth AuthR Auth getAuth
|]
```

We'll get to that AuthR bit a little later, but with the three routes 
defined we can create our handler functions for our various actions:

```haskell 
-- | Manage posts
getManagePosts/ :: Handler RepHtml
getManagePostsR = pageLayout $ do
    setTitle $ string "pbrisbin - Manage posts"
    addHamlet [$hamlet| %h1 Manage Posts |]
    runPostForm Nothing

postManagePostsR :: Handler RepHtml
postManagePostsR = getManagePostsR

-- | Edit post
getEditPostR :: String -> Handler RepHtml
getEditPostR slug = do
    post <- getPostBySlug slug
    case post of
        []        -> notFound
        (post':_) -> pageLayout $ do
            setTitle $ string "pbrisbin - Edit post"
            addHamlet [$hamlet| %h1 Edit Post |]
            runPostForm $ Just post'

postEditPostR :: String -> Handler RepHtml
postEditPostR = getEditPostR

-- | Delete post
getDelPostR :: String -> Handler RepHtml
getDelPostR slug = do
    deletePost slug
    setMessage $ [$hamlet| %em post deleted! |]
    redirect RedirectTemporary ManagePostsR
```

At least that part is pretty easy after all the upfront forms work.

Now, you can start up your test server and head to localhost:3000/manage 
to try it out.

![Manage Posts Screenshot](https://images.pbrisbin.com/posts_database/manage_posts.png)\ 

The problem now is that if you were push this live, everyone could mess 
with your data base. We need authentication.

## Authentication

This was the most difficult part of the whole thing.

There's very little documentation on `yesod-auth` and any real world 
examples I could find were for social-networking type authentication 
where every login box had "register for a new account" logic tied into 
it. This was not what I needed.

I wanted to store a username and hashed password in my new Persistent 
database and validate incoming users (just me) against that.

I ended up writing my own `AuthPlugin` modeled after 
`Yesod.Helpers.Auth.Email` which does just that.

I'm not going to go into the details, if you want to view the code it's 
on my 
[github](https://github.com/pbrisbin/devsite/blob/master/Helpers/Auth/HashDB.hs). 
I'd rather describe briefly how to use it.

<div class="well">
This has now been added to the main yesod-auth package.
</div>

After copying the source for the module into your app directory, make 
your site an instance of YesodAuth and put my plugin in your list:

```haskell 
instance YesodAuth DevSite where
    type AuthId DevSite = UserId

    -- these are site specific
    loginDest _  = ManagePostR
    logoutDest _ = RootR

    getAuthId    = getAuthIdHashDB AuthR 
    showAuthId _ = showIntegral
    readAuthId _ = readIntegral
    authPlugins  = [authHashDB]
```

That's where the /auth addition to `parseRoutes` is used.

Add the migration function to your controller:

```haskell 
withServer :: (Application -> IO a) -> IO a
withServer f = withConnectionPool $ \p -> do
    runSqlPool (runMigration migratePosts) p
    runSqlPool (runMigration migrateUsers) p -- right here
    let h = DevSite p
```

And that's it!

Well kinda. You'll need to add users to the database by hand, but that's 
not too hard, here's how I did mine:

    $ echo -n 'MySuperAwesomePassword' | sha1sum
    bf1bfb9af6e50018dacf19e1618e4fb5f981c14e  -
    $ sqlite3 posts.db3
    SQLite version 3.7.4
    Enter ".help" for instructions
    Enter SQL statements terminated with a ";"
    sqlite> insert into user (username,password) 
       ...> values ('pbrisbin','bf1bfb9af6e50018dacf19e1618e4fb5f981c14e');
    sqlite> .exit
    $

And it's incredibly easy to add authentication to any page, here's how I 
adjusted my management routes:

```haskell 
getManagePostsR :: Handler RepHtml
getManagePostsR = do
    _ <- requireAuth -- that's it, right there!

    postForm <- runPostForm Nothing
    pageLayout $ do
        ...
```

Now when you go to any page with authentication required, you're hit 
with a Login box:

![Login Screenshot](https://images.pbrisbin.com/posts_database/login_posts.png)\ 

Login and manage your posts.
