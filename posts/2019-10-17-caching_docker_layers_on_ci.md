---
title: Caching Docker Layers on CI
tags: docker
---

For as long as I've built Docker images on CI, I've fought the layer caching
problem. Working on Haskell projects of many dependencies, an un-cached
multi-stage build can take close to an hour. That's a deal-breaker for
deployments, where ten minutes is a reasonable maximum.

At some point, Circle quietly released a `docker_layer_caching` flag in their
`setup_remote_docker` Workflow step, and I happened to get the main [Restyled][]
image (`restyled/restyled.io`) into the beta. It was hit-or-miss generally, but
it's now being hard-blocked behind a very expensive pay wall -- hence my renewed
interest in alternatives.

[restyled]: https://restyled.io

For a time, I wired up a Rube Goldberg string of webhooks to get my images
building on Quay.io, because it supposedly offered good layer caching. After
seeing a 0% hit rate, I emailed support. They said, and I'm only paraphrasing
slightly here, "That's broken right now, and we have no timeline for fixing it."

Most recently, I went back to Docker Hub (or is it Docker Cloud?), because they
have a wonderful *Automated Builds* interface with a little "Cache Layers?"
check box; it seemed to work perfectly! The only issue?

1. Just having my Automated Builds trigger reliably at all was rare, and
1. There's no way to just build every push to a tag that is the git sha

I decided it was time to just handle the process myself. The answer was an
implementation of the *Remote Image Cache* section of [this blog post][post],
which I'll now detail.

[post]: https://blog.jondh.me.uk/2018/04/strategies-for-docker-layer-caching-in-circleci/

## The Basics

The basics of this solution are simple:

1. Pull the last image you built first
1. Build using `--cache-from <that>`
1. Push what you built, for next time

Pretty easy:

```sh
# Pull whatever you built last
docker pull restyled/restyled.io:x

# Build new, using prior work as much as possible
docker build \
  --tag restyled/restyled.io:x \
  --cache-from restyled/restyled.io:x \
  .

# Update the cache
docker push restyled/restyled.io:x

# Actual deployment
docker tag restyled/restyled.io:x restyled/restyled.io:prod
docker push restyled/restyled.io:prod
```

As usual, there is ample incidental complexity:

1. We probably want branch-specific tags, and a fallback to master

   Each PR should work with its own cache, and the first push should start with
   master as its cache.

1. Doing this with multi-stage builds is, *annoying*...

   See below.

## Multi-stage 101

For compiled software, it's common to use a *multi-stage* Docker build:

```dockerfile
# Stage 1
FROM fat-image-with-compiler-toolchain AS builder
RUN mkdir -p /src
WORKDIR /src

COPY src .
RUN make my-exe

# Stage 2
FROM slim-image
RUN mkdir -p /app
WORKDIR /app

COPY --from=builder /src/my-exe /app/
CMD ["/app/my-exe"]
```

This results in a slim final image, which only contains the executable (and
possibly other runtime libraries). And if you're doing all your builds on a
single machine, caching Just Works. But if you're trying to use `--cache-from`,
you can't push and pull just the final image, since it won't have any of the
`builder` layers (by design). Somehow, you have to separately build, push, and
pull the `builder` stage too.

### The Not-So-Basics

Accounting for said incidental complexity, here is what I'm *actually* doing...

Let's assume some inputs:

```sh
# Git branch name, sanitized for use as an image tag
branch=

# The (REGISTRY/)NAME(:TAG) you actually want to deploy
image=

# The (REGISTRY/)NAME image, without any TAG (hence _base)
image_base=
```

And the following two helper functions:

```sh
docker-pull-tag() {
  # Given remote and local image names, pull {remote}, then tag it as {local}
}

docker-tag-push() {
  # Given remote and local image names, tag {local} as {remote}, then push it
}
```

(Please forgive the hand-waving here, there are links to full source code at the
end.)

#### 1- Load Cache

First, try to pull an image tagged for our branch, then master, then entirely
un-prefixed. Stop on the first one to succeed. Each step tags the pulled image
to the same thing, so we can use that tag later, and we'll be working with the
most specific cache image we were able to find.

For each of these, we have to pull two images: one with the suffix `-builder`,
for our first stage, and another (un-suffixed) one for our runtime image. Wrap
it up in a function for the convenience of `return` and an overall `|| true` (so
as to not anger the `set -e` gods).

```sh
pull_cached() {
  echo ":: Pulling cached images for $branch"
  docker-pull-tag "$image_base:$branch-builder" "$image_base:builder" &&
    docker-pull-tag "$image_base:$branch" "$image_base" &&
    return 0

  echo ":: Pulling cached images for master"
  docker-pull-tag "$image_base:master-builder" "$image_base:builder" &&
    docker-pull-tag "$image_base:master" "$image_base" &&
    return 0

  echo ":: Pulling unprefixed cache images"
  docker-pull-tag "$image_base:builder" &&
    docker-pull-tag "$image_base"
}

pull_cached || true
```

#### 2- Build

Next, build the stages separately, so that we can store a cache image of each
stage. The second, runtime build also uses the first image as a cache source, so
this is negligibly slower than doing it all at once.

```sh
echo ":: Building builder image"
docker build \
  --tag "$image_base:builder" \
  --cache-from "$image_base:builder" \
  --target builder \
  "$@" \
  .

echo ":: Building image"
docker build \
  --tag "$image_base" \
  --cache-from "$image_base:builder" \
  --cache-from "$image_base" \
  "$@" \
  .
```

#### 3- Store Cache

Finally, push the cache images back. We always update our branch tags and the
un-prefixed ones. We don't update master here since we wouldn't want to do that
from a non-master branch. And if we're actually *on* master, then
`branch=master` anyway.

```sh
echo ":: Pushing cached images for $branch"
docker-tag-push "$image_base:builder" "$image_base:$branch-builder"
docker-tag-push "$image_base" "$image_base:$branch"

echo ":: Pushing unprefixed cached images"
docker-tag-push "$image_base:builder"
docker-tag-push "$image_base"
```

**NOTE**: I actually put this *before* the build step as a `trap`,

```sh
push_cached() {
  echo ":: Pushing cached images for $branch"
  docker-tag-push "$image_base:builder" "$image_base:$branch-builder"
  docker-tag-push "$image_base" "$image_base:$branch"

  echo ":: Pushing unprefixed cached images"
  docker-tag-push "$image_base:builder"
  docker-tag-push "$image_base"
}

trap 'push_cached || true' EXIT

echo ":: Building builder image"
# ...
```

If the second build fails, we would still preserve a cache of the first.

#### 4- Deploy

And then, do whatever it is you need to actually deploy the thing...

```sh
echo ":: Pushing final image"
docker-tag-push "$image_base" "$image"
```

## Installation & Usage

The scripts involved can be found [here][ops-files].

[ops-files]: https://github.com/restyled-io/ops/tree/v5/files/usr/local/bin

You'll just need the 3 `docker-` files on `$PATH`, then usage is:

```
docker-build-remote-cache <[REGISTRY/]NAME[:TAG]> [DOCKER BUILD OPTION...]
```

The script assumes you have two stages, and the first uses `AS builder`. I'd
gladly welcome a PR that makes that configurable, perhaps by script argument.

### Circle

If you're using Circle, and your current step isn't doing anything particularly
exotic, you should be able to use my actual `restyled/ops` image for this and
swap out `docker build` with `docker-build-remote-cache`:

```yaml
version: 2.0
jobs:
  image:
    docker:
      - image: restyled/ops:v5
    steps:
      - checkout
      - setup_remote_docker:
          version: 18.09.3
      - run:
          name: Build
          command: |
            docker login \
              -u "$DOCKERHUB_USERNAME" \
              -p "$DOCKERHUB_PASSWORD"

            # The goods 👇
            docker-build-remote-cache \
              "your-registry/$CIRCLE_PROJECT_NAME:$CIRCLE_SHA1"
```

You can also verify your build locally first. Within your repository, run:

```sh
docker run -it --rm \
  --volume /var/run/docker.sock:/var/run/docker.sock \
  --volume "$HOME"/.docker/config.json:/root/.docker/config.json:ro \
  --volume "$PWD":/build:ro \
  --workdir /build \
  restyled/ops:v5 docker-build-remote-cache <registry/image:tag>
```

The best part is the caching from this local build is re-usable on CI. If you
push to CI from the same branch, you should see a 100% cached build.

## Result

For `restyled/restyled.io`, where an un-cached build can take up to an hour,
most of my PRs have been finishing the `image` job in around 5 minutes. So
incurring the transfer cost of moving all the images around seems well worth it.
And by putting this all inside the `restyled/ops` image, which can be used
directly as a Circle job's environment, I can "port" this to all my projects
trivially.

![Using cache](https://images.pbrisbin.com/caching-docker-layers-on-ci/using-cache.png)\ 
