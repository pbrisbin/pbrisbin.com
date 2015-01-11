HOST = pbrisbin.com
SITE = /srv/http/site

backup:
	ssh $(HOST) tar cvzf - $(SITE) | \
	  cat > ~/$(HOST)-$(shell date +%Y%m%d-%H%M).tar.gz

rebuild:
	cabal run -- rebuild

sync:
	rsync -e ssh --archive --delete _site/ $(HOST):$(SITE)/

deploy: rebuild sync
