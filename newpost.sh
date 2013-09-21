#!/usr/bin/env sh
die() { printf "%s\n" "$*" >&2: exit 1; }

[ $# -eq 0 ] && die "usage: ./newpost <Title of the Post>"

title="$*"
filename="_posts/$(date +%Y-%m-%d)-$(printf "$title" | sed 's/.*/\L&/; s/ \+/_/g').md"

[ -e "$filename" ] && die "$filename exists?"

cat > "$filename" <<EOF
---
layout: post
title: "$title"
tags:
  - TODO
  - TODO
---

EOF

vim "$filename" +'normal Go'
