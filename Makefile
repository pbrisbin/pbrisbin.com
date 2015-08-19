SITE = _site
BUCKET = pbrisbin.com

setup:
	cabal update
	cabal sandbox init
	cabal install --dependencies-only

deploy:
	cabal run -- rebuild
	s3cmd \
	  --acl-public \
	  --delete-removed \
	  --no-mime-magic \
	  --cf-invalidate \
	  --cf-invalidate-default-index \
	  sync $(SITE)/ s3://$(BUCKET)

.PHONY: setup deploy
