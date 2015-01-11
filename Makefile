SITE = _site
BUCKET = pbrisbin.com

deploy:
	cabal run -- rebuild
	s3cmd sync $(SITE)/ s3://$(BUCKET)

.PHONY deploy
