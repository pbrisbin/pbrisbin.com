---
title: "An Opinionated Guide to Options Parsing in Shell"
tags: shell
---

Some may say that you shouldn't write shell beyond a certain, very low bar of
complexity. If you reach for arrays, certainly _associative_ arrays (gasp!), or
if your script approaches 20, 50, or 100 (how dare you!) lines, maybe you want a
"real" language.

Everyone's bar is different, but I'd wager actual options parsing is above it
for most. I think this is misguided; parsing options in shell can be valuable
enough, and done with low enough complexity, to more than pay for itself on this
scale. I think the problem is a lack of familiarity (did you even know you
_could_ parse options in shell?) coupled with confusing alternatives and an
information-dense (read: overwhelming) documentation style in the space.

I've arrived at a narrow pattern of shell options parsing that I think is
drastically improving my scripts, without introducing much by way of downside.
By accepting some limitations, I think I've found a good 80/20 in
benefit/complexity in this space.

### Skeleton

Here is how I begin any script I write:

```sh
#!/bin/sh
usage() {
  cat <<'EOM'
TODO
EOM
}

while getopts h opt; do
  case "$opt" in
    h)
      usage
      exit 0
      ;;
    \?)
      usage >&2
      exit 64
      ;;
  esac
done

shift $((OPTIND - 1))

printf '>%s<\n' "$@" # For demonstration purposes
```

I used to do this "when I needed", but I'm done fooling myself. I always end up
wanting this, and I'm always happy when I've done it from the start. Seeing
`usage` front-and-center top-of-file is great. Expecting `-h` in a script that
isn't used very often is extremely useful, for me and my team.

Let's break down what's happening:

```sh
while getopts h opt; do
```

The [`getopts`][man-getopts] program is typically a shell built-in and is
specified by POSIX. This means you can use it in pretty much any shell, but it
will be less featureful; no long options, for example. I prefer this over
[`getopt`][man-getopt], which is Bash-specific and does support long options. I
actually don't care too much about POSIX compatibility, and I most often write
scripts with a `bash` shebang, but I actually just find `getopt`'s usage very
clunky. Your mileage may vary.

[man-getopts]: https://man7.org/linux/man-pages/man1/getopts.1p.html
[man-getopt]: https://man7.org/linux/man-pages/man1/getopt.1.html

The `h` is the `optstring` or "options string". It defines the options you are
going to parse for. In this case, I'm saying the single option `h` without any
arguments. I'll extend it later and you'll see how its syntax works.

`opt` is the name of the variable that `getopts` will place each option it
parses into for each iteration of the loop.

```sh
case "$opt" in
  h)
    usage
    exit 0
    ;;
  \?)
    usage >&2
    exit 64
    ;;
esac
```

As mentioned, this will loop with `$opt` set to each (valid) flag we see, or `?`
if we were given something invalid. If given `h`, I print usage information to
`stdout` and exit successfully. The invalid branch is similar accept going to
`stderr` and exiting un-successfully.

I prefer to let `getopts` print its own error on invalid items,

```console
% ./example -h
TODO
% ./example -f
./example: illegal option -- f
TODO
% 64
```

I think its messages are perfectly clear and I'm happy to not manage them
myself. You can suppress these messages by prefixing the options string with
`:`. See the manpage for more details.

```sh
shift $((OPTIND - 1))

printf '>%s<\n' "$@"
```

Lastly, we `shift` passed the parsed options. That way, anything we don't handle
in `getopts` is `$@` at this point in the script:

```console
% ./example foo bar "baz bat"
>foo<
>bar<
>baz bat<
% ./example -f foo bar "baz bat"
./example: illegal option -- f
TODO
```

And since we're parsing options "for real" instead of adhoc, we get some
behavior for free, such as `--` to separate option-like arguments, needed to
support that last example:

```console
% ./example -- -f foo bar "baz bat"
>-f<
>foo<
>bar<
>baz bat<
```

## Flag options

Now, let's parse another option:

```sh
usage() {
  cat <<'EOM'
Usage: thing [-fh]

Options
  -f            Force the thing
  -h            Print this help

EOM
}

force=0

while getopts fh opt; do
  case "$opt" in
    f)
      force=1
      ;;
    # ...
  esac
done
```

Here you see one downside compared to "real" languages' options parsers: we have
to do things 3 times.

1. The argument to `getopts` contains `f`
1. The `case` statement must look for `f`
1. The `usage` function

If you configure [ShellCheck][] in your editor (you should!), that can at least
protect you from most mistakes in item 2:

![](https://images.pbrisbin.com/an_opinionated_guide_to_options_parsing_in_shell/shellcheck-not-handled.png)

[ShellCheck]: https://www.shellcheck.net/

## Options with arguments

Now, let's add an option with an argument:

```sh
usage() {
  cat <<'EOM'
Usage: thing [-fh] <-o PATH>

Options
  -f            Force the thing
  -o            Output file
  -h            Print this help

EOM
}

force=0
output=

while getopts fo:h opt; do
  case "$opt" in
    # ...
    o)
      output=$OPTARG
      ;;
    # ...
  esac
done

if [ -z "$output" ]; then
  echo "-o is required" >&2
  usage >&2
  exit 64
fi
```

As before the same 3 things:

1. Add `o:` to options string, the `:` indicates an argument is required
2. Look for `o` in the `case`; the argument will be present in `$OPTARG`
3. Document accordingly in `usage`

And we see a new downside: required options are on us to enforce.

This is certainly error-prone, but again, I'm shooting for the 80/20 on
complexity vs featureful-ness. If `getopts` somehow supported declaring options
as required, it would then need to also support defaulting. Going in this
direction can cause the complexity to spiral too far for POSIX.

For what it's worth, I agree with where they've drawn the line; and leaving that
to us makes defaulting pretty easy:

```sh
usage() {
  cat <<'EOM'
Usage: thing [-fh] [-o PATH]

Options
  -f            Force the thing
  -o            Output file, default is stdout
  -h            Print this help

EOM
}

output=/dev/stdout

while getopts # ...
```

## Complete example

This snippet should be a good copy-paste source for the limit of what POSIX
`getopts` provides:

```sh
#!/bin/sh
usage() {
  cat <<'EOM'
Usage: thing-mover [-fh] [-o PATH] [--] <THING> [THING...]
Move things into some output.

Options:
  -f            Overwrite output even if it exists
  -o            Output path, default is stdout
  -h            Show this help

Arguments:
  THING         Thing to move

EOM
}

force=0
output=/dev/stdout

while getopts fo:h opt; do
  case "$opt" in
    f)
      force=1
      ;;
    o)
      output=$OPTARG
      ;;
    h)
      usage
      exit 0
      ;;
    \?)
      usage >&2
      exit 64
      ;;
  esac
done

shift $((OPTIND - 1))

if [ $# -eq 0 ]; then
  echo "At least one thing is required" >&2
  usage >&2
  exit 64
fi

for thing in "$@"; do
  if thing_exists "$thing"; then
    if [ "$force" -ne 1 ]; then
      echo "Thing exists!" >&2
      exit 1
    fi
  fi

  move_thing "$thing" "$output"
done
```

**NOTE**: Normally I would just do nothing if no things were passed, as a form
of _define errors out of existence_[^1], but I'm enforcing the argument for
demonstration purposes here.

[^1]: [A Philosophy of Software Design](https://www.amazon.com/Philosophy-Software-Design-John-Ousterhout/dp/1732102201)
