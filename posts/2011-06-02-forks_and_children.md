---
title: Forks and Children
tags: linux
---

While writing a [small learning exercise in C][android-receiver], I came 
across a nifty little concept. The task itself was a common one: I 
wanted to spawn a subprocess to the background while letting the main 
process continue to loop.

[android-receiver]: https://github.com/pbrisbin/android-receiver

<div class=note>
Many thanks go to falconindy who spoon fed me quite a bit as I was 
wrapping my head around all of this knowledge I'm now shamelessly 
presenting as my own.
</div>

In most languages you have some facility to group code into a logical 
unit (a haskell function or a bash subshell) then pass that unit to a 
command which forks it off into the background for you (haskell's 
forkProcess or bash's simple `&`).

## Forking C

C takes a far different, but I'd say more elegant, approach. C provides 
a function, `fork()` which returns a `pid_t`.

The beauty of `fork()` is in its simplicity. All it does is create an 
exact copy of your program in its current state in memory. That's it.

```c 
int main() {
    pid_t pid;

    pid = fork();

    // ...
}
```

Guess what, now you've got two copies of your running program, both 
sitting at the exact spot where `pid` is being assigned the output of 
`fork()`.

In the copy that was the original (the parent), `pid` will be the 
process id of the *other* copy (the child). And in that child copy, 
`pid` will be assigned 0. That's it; the full extent of `fork()`.

So how do we use this?

Well, let's say you've got a program (as I did) which should sit and 
loop forever. When some event happens, we want to take some asynchronous 
action (in my case throw up a dzen notification).

This is the perfect time to use `fork()`. We'll let the main thread run 
continuously, and fork off a child to do its thing when the triggering 
event occurs.

Here's a simplified version:

```c 
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>

int main() {
    int ret;
    pid_t pid;

    while (1) {
       /* wait for the "event" */
       ret = some_blocking_process();

       if (ret) {
           /* fork it! */
           pid = fork();

           if (pid == 0) {
               /* we are the child, take action! */
               some_action(ret);
               exit(EXIT_SUCCESS);
           }

           /* and the parent loops forever... */
       }
    }
}
```

So as you can see, the main program waits until `some_blocking_process` 
returns an `int`. If that `int` is nonzero, we consider that "the event" 
so we fork to create a copy of ourselves. If `pid` is zero, we know we 
are the child process so we take `some_action` and then simply `exit`. 
The parent process will skip that if statement, loop again and wait for 
`some_blocking_process` to signal the next event.

## Zombie kids

So I may have lied to you slightly about the simplicity of this 
approach. The above is all well and good -- it is simple -- but I ran 
into a small snag while working with my little learner's app...

Zombies.

Turns out, when a child process exits, it reports its return value to 
its parent; like any good child should. The child does this by sending a 
`SIGCHLD` signal.

The parent then knows if all or some of its spawned children finished 
successfully or not. This is important if you've got some dependant 
logic or simply want to log that fact.

In my case, I couldn't care less. Succeed fail, whatever. I'm done with 
you kid -- go away.

Double turns out, if the parent neglects to act on the signal sent from 
the dying child, it can remain a zombie.

I think this is poor form. I mean, come on, a negligent parent is no 
reason to make a process wander around aimlessly as a zombie until the 
next reboot.

Ok, ok, enough with the metaphor. Bottom line -- all you need to do to 
prevent this is install a simple signal handler which will read (and 
ignore) the status of the child process in response to said signal.

Here's our same example, but this time with a simple handler added:

```c 
#define _GNU_SOURCE

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/wait.h>

static void sigchld_handler(int signum) {
    /* this just silences a compiler warning you might get since we 
     * discard the signum parameter that is passed in */
    (void) signum;

    /* the actual handling of the signal... */
    while (waitpid(-1, NULL, WNOHANG) > 0);
}

int main() {
    int ret;
    pid_t pid;

    struct sigaction sig_child;

    sig_child.sa_handler = &sigchld_handler;
    sigemptyset(&sig_child.sa_mask);
    sig_child.sa_flags = 0;
    sigaction(SIGCHLD, &sig_child, NULL);

    while (1) {
       ret = some_blocking_process();

       if (ret) {
           pid = fork();

           if (pid == 0) {
               some_action(ret);
               exit(EXIT_SUCCESS);
           }
       }
    }
}
```

That's it, no more zombies.

<div class="well">
I noticed in the source for `dzen2` that they use a double-fork approach 
which also prevents zombies -- with no need for signal handlers (yay 
KISS!):

```c 
if (fork() == 0) {
    if (fork() == 0) {
        //
        // child logic...
        //

        exit(EXIT_SUCCESS);
    }

    exit(EXIT_SUCCESS);
}

wait(0);

//
// continue parent logic...
//
```

I like this approach better.
</div>
