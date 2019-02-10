---
title: GoogleEmail2 Deprecation
tags: haskell, yesod
---

I maintain an Auth [plugin][oauth2-google] for authenticating with Google OAuth2
in a Yesod application. This plugin has always had functionality overlap with
the [`GoogleEmail2`][google-email2] plugin in the `yesod-auth` package. Our
Google plugin was present, removed (due to said overlap), then returned again,
along with [some discussion][google-pr] about deprecating `GoogleEmail2` in
favor of it. What was missing was the documentation for migrating.

[oauth2-google]: http://hackage.haskell.org/package/yesod-auth-oauth2-0.6.1.0/docs/Yesod-Auth-OAuth2-Google.html
[google-email2]: http://hackage.haskell.org/package/yesod-auth-1.6.5/docs/Yesod-Auth-GoogleEmail2.html
[google-pr]: https://github.com/thoughtbot/yesod-auth-oauth2/pull/32#issuecomment-110013684

Things lived happily in this state for some time, until the deprecation of the
Google+ API sparked the discussion again. `GooglEmail2` relies on this API, but
`OAuth2.Google` does not. Therefore, it makes more sense to push the ecosystem
towards our plugin. That means we need to document the migration path, which is
what this blog post is.

## Caveat

The following describes the fastest way to migrate your codebase, by changing as
little about your application as is required to maintain existing functionality
under the new plugin. However, I would consider it an introduction of Technical
Debt. I encourage you to spend the time to actually alter your application to
align better with how the new plugin does things. How to do that would be
application-specific, so I don't offer concrete guidance here -- but my hope is
that after following the "fast way" below, you will understand enough about the
differences between the plugins to know what to best do in your own codebase.

## Migration

Actually changing plugins is as simple as you might expect:

```diff
-import Yesod.Auth.GoogleEmail2
+import Yesod.Auth.OAuth2.Google

-authPlugins = [authGoogleEmailSaveToken clientId clientSecret]
+authPlugins = [oauth2GoogleScoped ["email", "profile"] clientId clientSecret]
```

This will result in:

1. The API token no longer being present in the session post-authentication
1. The `Creds` value seen in `authenticate` to differ

If neither of these matter to you (or are trivial to deal with in your
application), you are done.

Assuming that's not the case, the following is an example `authenticate`
function that masks these differences at that seam. That way, downstream code
shouldn't have to change:

```hs
-- OAuth2.Google provides the raw response from the /userinfo API call as a
-- field in credsExtra. By decoding that field to a value of this type, you can
-- access (hopefully) any data you were previously using from the credsExtra
-- fields set by GoogleEmail2. What fields you need, how you want to structure
-- this type, and your preferred JSON library are all up to you; this is just an
-- example
data GoogleUser
    = GoogleUser
    { name :: Text
    , email :: Text
    }
    deriving Generic

instance FromJSON GoogleUser

authenticate creds = do
    -- NOTE: I'm using unsafe pattern matching to keep the example clear. You
    -- should handle the Lefts and Nothings accordingly in your own code
    Right user <- getUserResponseJSON creds
    Just (AccessToken token) <- getAccessToken creds

    -- Address (1): save the token into the session
    setSession "_GOOGLE_ACCESS_TOKEN" token

    -- Address (2): build a Creds value like we were seeing before
    let updatedCreds = Creds
            { credsPlugin = "googleemail2"
            , credsIdent = email user
            , credsExtra =
                [ ("name", name user)
                -- And any other fields you were relying on. See below.
                ]
            }

  -- Proceed as before, with updatedCreds
```

This approach simplifies-- and makes explicit --the values you'll find in
`credsExtra`. This may or may not be problematic to your application, but it is
unavoidable. `GoogleEmail2` was requesting a `Person` resource from the
deprecated `/plus/v1/people/me` endpoint and serializing the entire JSON `Value`
into `[(Text, Text)]` in [an ad hoc way][allPersonInfo]. The former will stop
working some time in March and the latter is generally discouraged as a way of
handling data in Haskell.

[allPersonInfo]: http://hackage.haskell.org/package/yesod-auth-1.6.5/docs/src/Yesod.Auth.GoogleEmail2.html#allPersonInfo

For migration purposes, this `Person` resource is [much richer][person] and so
cannot be fully re-created from the simpler [`/userinfo`][userinfo] response
that `OAuth2.Google` provides:

[person]: https://developers.google.com/+/web/api/rest/latest/people#resource
[userinfo]: https://developers.google.com/apis-explorer/#p/oauth2/v2/oauth2.userinfo.get

```json
{
  "id": "999999999999999999999",
  "email": "you@gmail.com",
  "verified_email": true,
  "name": "Your Name",
  "given_name": "Your",
  "family_name": "Name",
  "link": "https://plus.google.com/999999999999999999999",
  "picture": "https://lh3.googleusercontent.com/...",
  "locale": "en"
}
```

If you were relying on data not present here, you will need to make [additional
API calls][people-api] to retrieve it.

[people-api]: https://developers.google.com/people/api/rest/v1/people
