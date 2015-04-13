SITE = _site
BUCKET = pbrisbin.com

deploy:
	cabal run -- rebuild
	s3cmd \
	  --acl-public \
	  --delete-removed \
	  --no-mime-magic \
	  --cf-invalidate \
	  --cf-invalidate-default-index \
	  sync $(SITE)/ s3://$(BUCKET)

.PHONY: deploy
