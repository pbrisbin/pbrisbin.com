---
title: Mocking Bash
tags: bash, testing, mocks, cram, aurget, arch
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
recorded response would be replayed instead, my script wouldn't know 
the difference and everything would pass reliably and instantly.

<div class="well">
In case you didn't notice, this is [VCR][].
</div>

[vcr]: https://github.com/vcr/vcr

The only limitation here is that a mock must be completely affective 
while only mimicking the `stdout`, `stderr`, and exit code of what it's 
mocking. A command that creates files, for example, which are used by 
other parts of the script could not be mocked this way.

## Mucking with PATH

One way to intercept calls to executables is to prepend `$PATH` with 
some controllable directory. Files placed in this leading directory will 
be found first in command lookups, allowing us to handle the calls.

I like to write my cram tests so that the first thing they do is source 
a `test/helper.sh`, so this makes a nice place to do such a thing:

**test/helper.sh**

```bash
export PATH="$TESTDIR/..:$TESTDIR/bin:$PATH"
```

This ensures that a) the executable in the source directory is used and 
b) anything in `test/bin` will take precedence over system commands.

Now all we have to do to mock `foo` is add a `test/bin/foo` which will 
be executed whenever our Subject Under Test calls `foo`.

## Record/Replay

The logic of what to do in a mock script is straight forward:

1. Build a unique identifier for the invocation
2. Look up a stored "response" by that identifier
3. If not found, run the program and record said response
4. Reply with the recorded response to satisfy the caller

We can easily abstract this in a generic, **12** line proxy:

**test/bin/act-like**

```bash 
#!/usr/bin/env bash
program="$1"; shift
base="${program##*/}"

fixtures="${TESTDIR:-test}/fixtures/$base/$(echo $* | md5sum | cut -d ' ' -f 1)"

if [[ ! -d "$fixtures" ]]; then
  mkdir -p "$fixtures"
  $program "$@" >"$fixtures/stdout" 2>"$fixtures/stderr"
  echo $? > "$fixtures/exit_code"
fi

cat "$fixtures/stdout"
cat "$fixtures/stderr" >&2

read -r exit_code < "$fixtures/exit_code"

exit $exit_code
```

With this in hand, we can record any invocation of anything we like (so 
long as we only need to mimic the `stdout`, `stderr`, and exit code).

**test/bin/curl**

```bash
#!/usr/bin/env bash
act-like /usr/bin/curl "$@"
```

**test/bin/makepkg**

```bash
#!/usr/bin/env bash
act-like /usr/bin/makepkg "$@"
```

**test/bin/pacman**

```bash
#!/usr/bin/env bash
act-like /usr/bin/pacman "$@"
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
