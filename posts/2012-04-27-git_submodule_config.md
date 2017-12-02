---
title: Git Submodule Config
tags: git
---

Git submodules are pretty great. They allow you to have nested git 
repositories so that modular parts of your app can exist as separate 
repos but be worked with as one file tree. Another benefit is that when 
submodules are pushed to github they appear simply as links to the repos 
they represent.

If your not familiar, go ahead and google then come back -- how 
submodules work overall is not the point of this post.

One of the ways I use submodules is to take modular pieces of my 
dotfiles repo and separate them out into single purpose, independently 
clonable repos for oh-my-zsh, vim and screen. A level down, inside the 
vim submodule itself, I use additional submodules in accordance with 
tpope's awesome pathogen plugin to bundle the various vim plugins I use. 
At both of these levels there exist submodules of which I am the author 
and an active developer.

When I work on these submodules, I like to do so from within the parent 
repo (vs independently in some other directory). This is especially 
important in vim so that I can test out my changes immediately. If I 
didn't do this, I would have to hack on the submodule, commit, push, go 
into the vim repo's copy and pull -- all before seeing the affects 
([Bret Victor][victor] would not be very happy with that workflow).

[victor]: http://vimeo.com/36579366 "Bret Victor - Inventing on Principle"

What this means is the submodule must be added with a pushable remote. 
And since I like to push using ssh keys and not enter my github 
credentials each time, I use the `git@github` url for that. Problem is, 
when someone wants to clone my repo (that's what it's there for), 
they're unable to recursively clone those submodules because they don't 
have access to them using that url. For that to work, I would've had to 
have added the submodules using the `https` protocol which allows 
anonymous checkouts.

As it turns out, due to the unexpected (but perfectly reasonable) 
behavior of a `git submodule add` command, I can actually have my cake 
and eat it too.

You see, when you do a `git submodule add <url> <directory>`, it writes 
that `url` to `.gitmodules`. This is the file and url that's used when 
you clone this repo anywhere else and init the submodules within. But 
this is **not** the url that's used when you actually try to push or 
pull from within the submodules!

In addition to `.gitmodules`, the url of the remote also gets written 
into the `.git/config` of the submodule as the `origin` remote (this is 
just normal `clone` behavior). *This* is the url that's used for 
push/pull from within the submodule. If you think about it, it makes 
perfect sense: you're in a valid git repo; when executing a `push`, you 
wouldn't expect it to use anything *but* the `remote` that was defined 
and stored in your `.git/config`.

<div class="well">
In some versions of git, I find that a submodule's `.git` is actually a 
file pointing to a `.git/modules/name/` directory in the parent repo.
</div>

Finally, the url/directory mapping for the submodule also gets written 
into the parent repo's `.git/config`. What purpose does that serve? If 
you figure it out, let me know...

So (however unlikely this is) if you find yourself in the same situation 
as I, *this is how you do that*:

```bash 
$ git submodule add https://github.com/you/repo some/dir
$ git commit -m 'added submodule repo'
$ cd some/dir
$ git remote set-url origin git@github.com:you/repo
```

Now anyone who clones (recursively) will get the anonymous checkout 
version (as defined in `.gitmodules`), but the `origin` remote in the 
local submodule has been changed to the `git@github` version and is 
pushable using ssh keys.

<div class="well">
I recently discovered that this can be solved much more elegantly by 
adding the following to *~/.gitconfig*:

```
[url "git@github.com:pbrisbin/"]
  pushInsteadOf = "https://github.com/pbrisbin/"
```

Now whenever `git` encounters the anonymous http remote, it'll silently 
use the ssh-based url. Aces.
</div>
