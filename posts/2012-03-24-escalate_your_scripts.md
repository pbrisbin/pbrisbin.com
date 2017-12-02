---
title: Escalate Your Scripts
tags: ruby
---

Anyone who knows me knows I love the shell. I got my "start" in bash and 
still have a plethora of scripts lying around doing all sorts of useful 
and fun things for me. Recently, however, I tackled a task that I had 
attempted many times in shell script always to be met with frustration. 
How did I finally figure it out? I made it a `rake` task and did it in 
ruby.

Ask me last month what I thought the best tool for this job would've 
been, and 99 times out of 100 I would've said "shell script". But guess 
what, I couldn't do it -- just never worked out. Now, after having 
written quite a nice little `Rakefile`, I can say confidently that I 
wish I had thought to do this sooner -- and I hope I'll think to do it 
again.

I want to write about this exercise mainly because I found the process 
to be quite enjoyable. When I needed to do imperative flow control, call 
system commands, and move things about the file system, I felt no 
resistance. More importantly, I could use all of the higher-level 
features to keep the code clear and clean.

And this is not just praise for ruby (though it does a good job), I'm 
more recommending that when presented with a task that makes sense as a 
shell script -- think for a second if it might not be possible to do in 
a higher-level language, you might be surprised.

## The Problem

I've got a repo (as a lot of you probably do) that contains my main 
dotfiles. It's a collection of files that are usually scattered 
throughout my home directory which I've centralized into one folder and 
placed under version control. The normal approach with this is to 
symlink these files from the central location out into the proper places 
under `$HOME`.

I wanted to automate this process. I wanted to be able to setup a new 
box by cloning this repo and running a single script. After that script 
completes, I want as much of my environment as is generally applicable 
to be fully configured.

The challenges here were that not all of the files in the repo made 
sense on every machine, some required parent directories to exist and, 
of course, I had to be careful not to clobber anything already present.

Nothing about this is insurmountable; the (albeit self-imposed) 
challenge is to do it as simply and maintainably as possible.

## Objectify

The interesting thing about this script is what parts are higher level 
and what parts are not. So first, here are all of the higher-level bits 
with the scriptier parts left out:

```ruby 
require 'fileutils'

module Dotfiles
  def self.each(&block)
    [
      '.xcolors/jasonwryan.xcolors',
      '.xcolors/zenburn.xcolors',
      '.gitconfig',
      '.gitignore',
      '.htoprc',
      '.dir_colors',
      '.Xdefaults',
      '.zshrc',
      '.oh-my-zsh',
      '.screen',
      '.vim'
    ].each do |file|
      yield Dotfile.new(file)
    end
  end

  class Dotfile
    include FileUtils

    attr_reader :dotfile
    attr_accessor :source, :target

    def initialize(dotfile)
      @dotfile = dotfile
      @source  = File.join(pwd, dotfile)
      @target  = File.join(ENV['HOME'], dotfile)
    end

    def install!

      #
      # ...
      #

    end
  end
end

desc "updates all submodules"
task :submodules do

  #
  # ...
  #

end

desc "installs all dotfiles into the proper places"
task :install => [:submodules] do

  #
  # ...
  #

end

task :default => :install
```

This shows the pattern I most often follow when scripting in ruby (which 
is very different than programming in ruby): one, top-level module to 
hold any script-wide logic or constants as well as classes to represent 
the data your working with.

With an overall module and a clean API of classes and methods, you 
provide yourself a useful set of commands above and beyond the flow 
control and backtick-interpolation you would normally lean on.

You'll also notice, in that `each` method, something I'm calling a 
Parallel Good Decision. I decided to hardcode the list of dotfile paths 
relative to the repo. This solved a number of problems that were leading 
to very smelly code. I could've used `git ls-files` or a normal 
glob-and-blacklist approach, but simply hardcoding this list allows 
finer control over what files are linked and if they are treated as 
files or directories.

Had I made this decision in isolation, it might have been enough for me 
to get that shell script approach working -- but I didn't. For some 
reason, only when cleaning up everything else and approaching the 
problem from a (slightly) higher level did I see that a simple list of 
relative file paths made the most sense here.

## Script It Out

Now that the skeleton-slash-library code is in place, we can fill in the 
gaps:

```ruby 
require 'fileutils'

module Dotfiles
  def self.each(&block)
    # ... 
  end

  class Dotfile
    # ...

    def install!
      puts "--> installing #{dotfile} as #{target}..."
      if File.exists?(target)
        if File.symlink?(target)
          rm target, :verbose => true
        else
          mv target, "#{target}.backup", :verbose => true
        end
      end

      ln_s source, target, :verbose => true
    end
  end
end

desc "updates all submodules"
task :submodules do
  unless system('git submodule update --init --recursive')
    raise 'error initializing submodules'
  end
end

desc "installs all dotfiles into the proper places"
task :install => [:submodules] do
  Dotfiles.each(&:install!)

  vimrc = Dotfiles::Dotfile.new('.vimrc')
  vimrc.source = File.join(ENV['HOME'], '.vim', 'vimrc')
  vimrc.install!
end

task :default => :install
```

The stuff that's easy is easy, the stuff that's hard is easier and 
overall, the code is very clean and maintainable.

Oh, and I guess it's nice that it works.
