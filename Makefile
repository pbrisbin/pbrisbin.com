all: setup build lint site

.PHONY: setup
setup:
	stack build --dependencies-only
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build --pedantic

.PHONY: lint
lint:
	stack exec hlint Main.hs
	stack exec weeder .

.PHONY: site
site:
	stack exec site build

S3CMD ?= s3cmd

.PHONY: diff
diff:
	mkdir _released || $(RM) -r _released/*
	$(S3CMD) sync s3://pbrisbin.com _released
	diff --color=always --unified _released _site


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
	stack exec site clean

.PHONY: watch
watch:
	stack exec site watch

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
