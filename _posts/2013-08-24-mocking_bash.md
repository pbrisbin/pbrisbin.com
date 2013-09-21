---
layout: post
title: "Mocking Bash"
tags:
  - bash
  - testing
  - mocks
  - cram
  - aurget
  - arch
---

Have you ever wanted to mock a program on your system so you could write 
fast and reliable tests around a shell script which calls it? Yeah, I 
didn't think so.

Well I did, so here's how I did it.

## Cram

Verification testing of shell scripts is surprisingly easy. Thanks to 
Unix, most shell scripts have limited interfaces with their environment. 
Assertions against `stdout` can often be enough to verify a script's 
behavior.

One tool that makes these kind of executions and assertions easy is 
[cram].

[cram]: https://bitheap.org/cram/

Cram's mechanics are very simple. You write a test file like this:

```
The ls command should print one column when passed -1

  $ mkdir foo
  > touch foo/bar
  > touch foo/baz

  $ ls -1 foo
  bar
  baz

```

Any line beginning with an indented `$` is executed (with `>` allowing 
multi-line commands). The indented text below such commands is compared 
with the actual output at that point. If it doesn't match, the test 
fails and a contextual diff is shown.

With this philosophy, retrofitting tests on an already working script is 
incredibly easy. You just put in a command, run the test, then insert 
whatever the actual output was as the assertion. Cram's `--interactive` 
flag is meant for exactly this. Aces.

## Not Quite

Suppose your script calls a program internally whose behavior depends on 
transient things which are outside of your control. Maybe you call 
`curl` which of course depends on the state of the internet between you 
and the server you're accessing. With the output changing between runs, 
these tests become more trouble than they're worth.

What'd be really great is if I could do the following:

1. Intercept calls to the program
2. Run the program normally, but record "the response"
3. On subsequent invocations, just replay the response and don't call 
   the program

This means I could run the test suite once, letting it really call the 
program, but record the `stdout`, `stderr`, and exit code of the call. 
The next time I run the test suite, nothing would actually happen. The 
recorded response would be replayed in stead, my script wouldn't know 
the difference and everything would pass reliably and instantly.

<div class="note">
In case you didn't notice, this is [VCR][].
</div>

[vcr]: https://github.com/vcr/vcr

The only limitation here is that a mock must be completely affective 
while only mimicking the `stdout`, `stderr`, and exit code of what it's 
mocking. A command that creates files, for example, which are used by 
other parts of the script could not be mocked this way.

## The Source, Luke

One way to intercept calls to executables is to declare a function by 
the same name. Unfortunately, this only works if the program logic (the 
caller of the executable) is in the same script as the function 
definitions. Fortunately, any script can be easily modified so that, in 
stead of being called directly, it can be sourced into another and 
called via an entry function.

First, move the "active" part of the script (meaning not a function 
definition or variable assignment) into a new function, and replace it 
with a call to that function.

So,

```bash 
# program functions...

initialize "$@"

if searching; then
  perform_search
else
  upgrading && add_available_upgrades
  resolving && resolve_dependencies

  setup_targets
  process_targets
fi
```

Becomes:

```bash 
# program functions...

main() {
  initialize "$@"

  if searching; then
    perform_search
  else
    upgrading && add_available_upgrades
    resolving && resolve_dependencies

    setup_targets
    process_targets
  fi
}

main "$@"
```

Then just make it so that function is only called when you want it to 
be. In our case, we want to prevent the call if we're in a cram test:

```bash 
main() {
  # ...
}

[[ -n "$TESTDIR" ]] && return

main "$@"
```

Now, actual invocations like this:

```
$ my-script --foo --bar
Expected output
```

Can be accurately reflected in a test like this:

```
  $ source my-script

Test foo and bar

  $ main --foo --bar
  Expected output

```

Except that we now have a chance to do things in between the `source` 
and the `main` -- like define functions to modify script behavior.

## Halp

To easily override commands with same-named functions, we'll move things 
into a helper file:

**test/helper.sh**

```bash 
curl() {
  echo "zomg you called curl with $*"
}

source "$TESTDIR/../my-script"
```

And source it at the top of every cram test:

```
  $ source "$TESTDIR/helper.sh"

Test the thing...
```

## Record/Replay

The logic of what to do in that `curl` function is pretty simple:

1. Build a unique identifier for the invocation

2. Look up a stored "response" by that identifier

3. If not found, run the program and record a response

4. Reply with the recorded response to satisfy the caller

All this in **11** lines:

**test/bin/mock**

```bash 
#!/usr/bin/env bash
program="$1"; shift

fixtures="$TESTDIR/fixtures/$program/$(echo $* | md5sum | cut -d ' ' -f 1)"

if [[ ! -d "$fixtures" ]]; then
  mkdir -p "$fixtures"
  $(which $program) "$@" >"$fixtures/stdout" 2>"$fixtures/stderr"
  echo $? > "$fixtures/exit_code"
fi

read -r exit_code < "$fixtures/exit_code"

cat "$fixtures/stdout"
cat "$fixtures/stderr" >&2
exit $exit_code
```

With this generalized proxy in hand, we can record any invocation of 
anything we like (so long as we only need to mimic the `stdout`, 
`stderr`, and exit code).

```bash 
curl()    { "$TESTDIR/bin/mock" curl    "$@"; }
makepkg() { "$TESTDIR/bin/mock" makepkg "$@"; }
pacman()  { "$TESTDIR/bin/mock" pacman  "$@"; }

source "$TEST_DIR/../my-script"
```

## Success!

After my next test run, I find the following:

```
$ tree test/fixtures
test/fixtures
├── curl
│   ├── 008f2e64f6dd569e9da714ba8847ae7e
│   │   ├── exit_code
│   │   ├── stderr
│   │   └── stdout
│   ├── 2c5906baa66c800b095c2b47173672ba
│   │   ├── exit_code
│   │   ├── stderr
│   │   └── stdout
│   ├── c50061ffc84a6e1976d1e1129a9868bc
│   │   ├── exit_code
│   │   ├── stderr
│   │   └── stdout
│   ├── f38bb573029c69c0cdc96f7435aaeafe
│   │   ├── exit_code
│   │   ├── stderr
│   │   └── stdout
│   ├── fc5a0df540104584df9c40d169e23d4c
│   │   ├── exit_code
│   │   ├── stderr
│   │   └── stdout
│   └── fda35c202edffac302a7b708d2534659
│       ├── exit_code
│       ├── stderr
│       └── stdout
├── makepkg
│   └── 889437f54f390ee62a5d2d0347824756
│       ├── exit_code
│       ├── stderr
│       └── stdout
└── pacman
    └── af8e8c81790da89bc01a0410521030c6
        ├── exit_code
        ├── stderr
        └── stdout

11 directories, 24 files
```

Each hash-directory, representing one invocation of the given program, 
contains the full response in the form of `stdout`, `stderr`, and 
`exit_code` files

I run my tests again. This time, rather than calling any of the actual 
programs, the responses are found and replayed. The tests pass 
instantly.
