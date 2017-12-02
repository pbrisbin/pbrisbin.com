---
title: MapToggle
tags: vim
---

This snippet, when added to one's `~/.vimrc`, allows the toggling of commonly
used options (i.e. things like `hls` or `wrap`) with a single keypress.

First, you'll have to define the actual function:

```vim
function! MapToggle(key, opt)
  let cmd = ':set '.a:opt.'! \| set '.a:opt."?\<CR>"
  exec 'nnoremap '.a:key.' '.cmd
  exec 'inoremap '.a:key." \<C-O>".cmd
endfunction

command! -nargs=+ MapToggle call MapToggle(<f-args>)
```

Then, map keys to that function:

```vim
MapToggle <F4> foldenable
MapToggle <F5> number
MapToggle <F6> spell
MapToggle <F7> paste
MapToggle <F8> hlsearch
MapToggle <F9> wrap
```

You'll even get a nice notification in your vim command prompt when you toggle
the setting
