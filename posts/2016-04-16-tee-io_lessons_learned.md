---
title: tee-io Lessons Learned
tags: haskell, yesod
---

A while back, I launched a side project called [tee-io][]. It's sort of like a
live pastebin. You use its [API][] to create a *command* and then send it
buffered *output*, usually a line at a time. Creating the command gives you a
URL where you can watch the output come in in real time. We use it at work to
monitor the commands run by our bot instead of waiting for the (potentially
long) command to finish and report all the output back to us at once.

[tee-io]: https://tee-io.herokuapp.com
[api]: http://docs.teeio.apiary.io/#

![tee-io-in-action](https://tee-io.herokuapp.com/static/demo.gif)\

While working on this project, which is built with Yesod, I started to settle on
some conventions for things I've not seen written up in the wild. I'd like to
collect my thoughts here, both for myself and in case these conventions are
useful to others.

## Worker

One thing tee-io does that I think is common but under-served in the tutorial
space is background work. In addition to the main warp-based binary, it's often
necessary to run something on a schedule and do periodic tasks. In tee-io's
case, I want to archive older command output to S3 every 10 minutes.

My approach is to define a second executable target:

```
executable              tee-io-worker
    if flag(library-only)
        buildable:      False

    main-is:            main-worker.hs
    hs-source-dirs:     app
    build-depends:      base
                      , tee-io

    ghc-options:        -Wall -Werror -threaded -O2 -rtsopts -with-rtsopts=-N
```

This is basically a copy-paste of the existing executable, and the
implementation is also similar:

```haskell
import Prelude (IO)
import Worker (workerMain)

main :: IO ()
main = workerMain
```

`workerMain` uses the "unsafe" `handler` function to run a `Handler` action as
`IO`:

```haskell
workerMain :: IO ()
workerMain = handler $ do
    timeout <- appCommandTimeout . appSettings <$> getYesod
    archiveCommands timeout

archiveCommands :: Second -> Handler ()
archiveCommands timeout = runDB $ -- ...
```

Making the heavy lifting a `Handler ()` means I have access to logging, the
database, and any other configuration present in a fully-inflated `App` value.
It's certainly possible to write this directly in `IO`, but the only real
downside to `Handler` is that if I accidentally try to do something request or
response-related, it won't work. In my opinion, pragmatism outweighs principle
in this case.

## Logging

One of the major functional changes I make to a scaffolded Yesod project is
around `AppSettings`, and specifically logging verbosity.

I like to avoid the `#define DEVELOPMENT` stuff as much as possible. It's
required for template-reloading and similar settings because there's no way to
give the functions that need to know those settings an `IO` context. For
everything else, I prefer [environment variables][12-factor].

[12-factor]: http://12factor.net/

In keeping with that spirit, I replace the compile-time, logging-related
configuration fields with a single, env-based `log-level`:

**Settings.hs**

```haskell
instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let appStaticDir = "static"
        appDatabaseConf <- fromDatabaseUrl
            <$> o .: "database-pool-size"
            <*> o .: "database-url"
        appRoot <- o .: "approot"
        appHost <- fromString <$> o .: "host"
        appPort <- o .: "port"
        appIpFromHeader <- o .: "ip-from-header"
        appCommandTimeout <- fromIntegral
            <$> (o .: "command-timeout" :: Parser Integer)
        S3URL appS3Service appS3Bucket <- o .: "s3-url"
        appMutableStatic <- o .: "mutable-static"

        appLogLevel <- parseLogLevel <$> o .: "log-level"
        -- ^ here

        return AppSettings{..}

      where
        parseLogLevel :: Text -> LogLevel
        parseLogLevel t = case T.toLower t of
            "debug" -> LevelDebug
            "info" -> LevelInfo
            "warn" -> LevelWarn
            "error" -> LevelError
            _ -> LevelOther t
```

**config/settings.yml**

```yaml
approot: "_env:APPROOT:http://localhost:3000"
command-timeout: "_env:COMMAND_TIMEOUT:300"
database-pool-size: "_env:PGPOOLSIZE:10"
database-url: "_env:DATABASE_URL:postgres://teeio:teeio@localhost:5432/teeio"
host: "_env:HOST:*4"
ip-from-header: "_env:IP_FROM_HEADER:false"
log-level: "_env:LOG_LEVEL:info"
mutable-static: "_env:MUTABLE_STATIC:false"
port: "_env:PORT:3000"
s3-url: "_env:S3_URL:https://s3.amazonaws.com/tee.io"
```

I don't use `config/test-settings.yml` and prefer to inject whatever variables
are appropriate for the given context directly. To make that easier, I load
`.env` files through my [load-env][] package in the [appropriate][dev-load]
[places][test-load].

[load-env]: http://hackage.haskell.org/package/load-env
[dev-load]: https://github.com/pbrisbin/tee-io/blob/9f77f9707e28439f2cb15cd3de41f2a8d73d3c3b/src/Application.hs#L125
[test-load]: https://github.com/pbrisbin/tee-io/blob/9f77f9707e28439f2cb15cd3de41f2a8d73d3c3b/test/SpecHelper.hs#L45

**.env** (development)

```bash
COMMAND_TIMEOUT=5
LOG_LEVEL=debug
MUTABLE_STATIC=true
S3_URL=https://s3.amazonaws.com/tee.io.development
```

**.env.test**

```bash
DATABASE_URL=postgres://teeio:teeio@localhost:5432/teeio_test
LOG_LEVEL=error
S3_URL=http://localhost:4569/tee.io.test
```

Now I can adjust my logging verbosity in production with a simple `heroku
config:set`, whereas before I needed a compilation and deployment to do that!

Yesod applications log in a few different ways, so there are a handful of
touch-points where we need to check this setting. To make that easier, I put a
centralized helper alongside the data type in `Settings.hs`:

```haskell
allowsLevel :: AppSettings -> LogLevel -> Bool
AppSettings{..} `allowsLevel` level = level >= appLogLevel
```

The first place to use it is the `shouldLog` member of the `Yesod` instance:

```haskell
shouldLog App{..} _source level = appSettings `allowsLevel` level
```

Second is the logging middleware. It's a little tricky to get the right behavior
here because, with the default scaffold, this logging always happens. It has no
concept of level and wasn't attempting to make use of `shouldLog` in any way.

The approach I landed on was to change the destination to (basically)
`/dev/null` if we're not logging at `INFO` or lower. That's equivalent to if
these messages were tagged `INFO` and respected our configured level, which
seems accurate to me. The big win here is they no longer mess up my test suite
output.

```haskell
makeLogWare foundation = mkRequestLogger def
    { outputFormat = if appSettings foundation `allowsLevel` LevelDebug
        then Detailed True
        else Apache apacheIpSource
    , destination = if appSettings foundation `allowsLevel` LevelInfo
        then Logger $ loggerSet $ appLogger foundation
        else Callback $ \_ -> return ()
    }
```

One last thing, specific to tee-io, is that I can use this setting to turn on
debug logging in the AWS library I use:

```haskell
logger <- AWS.newLogger (if appSettings foundation `allowsLevel` LevelDebug
    then AWS.Debug
    else AWS.Error) stdout
```

It's pretty nice to set `LOG_LEVEL=debug` and start getting detailed logging for
all AWS interactions. Kudos to [amazonka][] for having great logging too.

[amazonka]: http://hackage.haskell.org/package/amazonka

## REPL-Driven-Development

`DevelMain.hs` has quickly become my preferred way to develop Yesod
applications. This file ships with the scaffold and defines a module for
starting, stopping, or reloading an instance of your development server directly
from the REPL:

```
stack repl --ghc-options="-DDEVELOPMENT -O0 -fobject-code"
λ> :l DevelMain
DevelMain.update
Devel application launched: http://localhost:3000
```

The big win here in my opinion is that, in addition to viewing changes in your
local browser, you naturally fall into a REPL-based workflow. It's not something
I was actively missing in Yesod projects, but now that I'm doing it, it feels
really great.

I happen to have a nice `Show` instance for my settings, which I can see with
`handler`:

```
λ> appSettings <$> handler getYesod
log_level=LevelDebug host=HostIPv4 port=3000 root="http://localhost:3000"
  db=[user=teeio password=teeio host=localhost port=5432 dbname=teeio]
  s3_bucket=tee.io.development command_timeout=5s
```

<small>
*(Line breaks added for readability, here and below.)*
</small>

And I can investigate or alter my local data easily with `db`:

```
λ> db $ selectFirst [] [Desc CommandCreatedAt]
Just (Entity
      { entityKey = CommandKey
          { unCommandKey = SqlBackendKey {unSqlBackendKey = 1097} }
      , entityVal = Command
          { commandToken = Token {tokenUUID = e79dae2c-020e-48d4-ac0b-6d9c6d79dbf4}
          , commandDescription = Just "That example command"
          , commandCreatedAt = 2016-02-11 14:50:19.786977 UTC
          }
      })
λ>
```

Finally, this makes it easy to locally test that worker process:

```
λ> :l Worker
λ> workerMain
16/Apr/2016:14:08:28 -0400 [Debug#SQL]
  SELECT "command"."id", ...
    FROM "command"
  LEFT OUTER JOIN "output"
     ON ("command"."id" = "output"."command")
    AND ("output"."created_at" > ?)
  WHERE (("command"."created_at" < ?)
    AND ("output"."id" IS NULL))
  ; [ PersistUTCTime 2016-04-16 18:08:23.903484 UTC
    , PersistUTCTime 2016-04-16 18:08:23.903484 UTC
    ]
16/Apr/2016:14:08:28 -0400 [Info] archive_commands count=1
  @(main:Worker /home/patrick/code/pbrisbin/tee-io/src/Worker.hs:37:7)
[Client Request] {
  host      = s3.amazonaws.com:443
  secure    = True
  method    = PUT
  target    = Nothing
  timeout   = Just 70000000
  redirects = 0
  path      = /tee.io.development/b9a74a98-0b16-4a23-94f1-5df0a01667d0
  query     = 
  headers   = ...
  body      = ...
}
[Client Response] {
  status  = 200 OK
  headers = ...
}
16/Apr/2016:14:08:28 -0400 [Debug#SQL] SELECT "id", "command", ...
16/Apr/2016:14:08:28 -0400 [Debug#SQL] DELETE FROM "output" WHERE ...
16/Apr/2016:14:08:28 -0400 [Debug#SQL] DELETE FROM "command" WHERE ...
16/Apr/2016:14:08:28 -0400 [Info] archived token=b9a74a98-0b16-4a23-94f1-5df0a01667d0
  @(main:Worker /home/patrick/code/pbrisbin/tee-io/src/Worker.hs:59:7)
λ>
```

Since I run with `DEBUG` in development, and that was picked up by the REPL, we
can see all the S3 and database interactions the job goes through.

The console was one of the features I felt was lacking when first coming to
Yesod from Rails. I got used to not having it, but I'm glad to see there have
been huge improvements in this area while I wasn't paying attention.

## Deployment

I've been watching the deployment story for Yesod and Heroku change drastically
over the past few years. From [compiling on a VM][heroku-1], to a [GHC build
pack][heroku-2], to [Halcyon][heroku-3], the experience hasn't exactly been
smooth. Well, it seems I might have been right in the conclusion of that last
blog post:

[heroku-1]: https://pbrisbin.com/posts/deploying_yesod_apps_on_heroku/
[heroku-2]: https://brianmckenna.org/blog/haskell_buildpack_heroku
[heroku-3]: https://robots.thoughtbot.com/building-haskell-projects-with-halcyon#deployment

> Docker [...] could solve these issues in a complete way by accident.

We now have a Heroku plugin for using Docker to build a slug in a container
identical to their Cedar infrastructure, then extracting and releasing it via
their API.

Everything we ship at work is Docker-based, so I'm very comfortable with the
concepts and machine setup required (which isn't much), so using this release
strategy for my Yesod applications has been great. Your mileage may vary though:
while I do feel it's the best approach available today, there may be some bumps
and yaks for those not already familiar with Docker -- especially if on an
unfortunate operating system, like OS X.

Thanks to the good folks at thoughtbot, who are maintaining a [base image][base]
for releasing a stack-based project using this Heroku plugin, making tee-io
deployable to Heroku looked like this:

[base]: https://github.com/thoughtbot/docker-heroku-haskell-stack

```console
% cat Procfile
web: ./tee-io

% cat app.json
{
  "name": "tee.io",
  "description": "This is required for heroku docker:release"
}

% cat docker-compose.yml
# This is required for heroku docker:release
web:
  build: .

% cat Dockerfile
FROM thoughtbot/heroku-haskell-stack:lts-5.12
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>
```

And I just run:

```
heroku docker:release
```

And that's it!

If you're interested in seeing any of the code examples here in the context of
the real project, checkout the [tee-io source][source] on GitHub.

[source]: https://github.com/pbrisbin/tee-io
