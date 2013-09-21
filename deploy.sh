#!/usr/bin/env sh
set -e

remote="162.243.3.84"
timestamp="$(date +%Y%m%d%H%M)"

cd "$HOME/Code/pbrisbin"

jekyll build

tar czf - _site | ssh "$remote" 'tar xzf -'

ssh -t "$remote" "
  mv _site '/srv/http/site/releases/$timestamp'
  rm /srv/http/site/current
  ln -s 'releases/$timestamp' '/srv/http/site/current'
"
