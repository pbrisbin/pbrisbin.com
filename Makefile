HOST = pbrisbin.com
SITE = /srv/http/site

backup:
	ssh $(HOST) tar cvzf - $(SITE) | \
	  cat > ~/$(HOST)-$(shell date +%Y%m%d-%H%M).tar.gz

deploy:
	cabal run -- rebuild
	rsync -e ssh --archive _site/ $(HOST):$(SITE)/
