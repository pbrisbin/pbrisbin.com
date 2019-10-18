all: setup build lint site

STACK ?= stack

.PHONY: setup
setup:
	$(STACK) build --dependencies-only
	$(STACK) install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	$(STACK) build --pedantic

.PHONY: lint
lint:
	$(STACK) exec hlint Main.hs
	$(STACK) exec weeder .

.PHONY: site
site:
	$(STACK) exec site build

S3CMD ?= s3cmd

.PHONY: diff
diff: build clean site
	mkdir _released || $(RM) -r _released/*
	$(S3CMD) sync s3://pbrisbin.com _released
	diff --unified _released _site


.PHONY: publish
publish:
	$(S3CMD) \
	  --acl-public \
	  --delete-removed \
	  --no-mime-magic \
	  --cf-invalidate \
	  --cf-invalidate-default-index \
	  sync _site/ s3://pbrisbin.com/

.PHONY: clean
clean:
	$(STACK) exec site clean

.PHONY: watch
watch:
	$(STACK) exec site watch

TITLE ?= "" # causes tab-completion

_POST_DATE  ?= $(shell date +%Y-%m-%d)
_POST_TITLE ?= $(shell echo "$(TITLE)" | sed 's/'\''//g; s/ \+/_/g; s/.*/\L&/g')
_POST_PATH  ?= posts/$(_POST_DATE)-$(_POST_TITLE).md

.PHONY: new
new:
	[ -n "$(TITLE)" ]
	[ ! -f "$(_POST_PATH)" ]
	@printf "%s\n" \
	  "---" \
	  "title: \"$(TITLE)\"" \
	  "tags:" \
	  "---" "" > "$(_POST_PATH)"
	@echo CREATED: $(_POST_PATH)
