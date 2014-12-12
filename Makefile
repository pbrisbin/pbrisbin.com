HOST = pbrisbin.com
SITE = /srv/http/site

backup:
	ssh $(HOST) tar cvzf - $(SITE) | \
	  cat > ~/$(HOST)-$(shell date +%Y%m%d-%H%M).tar.gz

rebuild:
	cabal run -- rebuild

check: rebuild
	rsync -e ssh --archive --delete $(HOST):$(SITE)/ _current/
	diff -r _current _site

sync:
	rsync -e ssh --archive --delete _site/ $(HOST):$(SITE)/

deploy: rebuild sync
