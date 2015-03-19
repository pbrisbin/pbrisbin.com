SITE = _site
BUCKET = pbrisbin.com

deploy:
	cabal run -- rebuild
	s3cmd \
	  --cf-invalidate \
	  --cf-invalidate-default-index \
	  sync $(SITE)/ s3://$(BUCKET)

.PHONY: deploy
