TITLE ?= "" # causes tab-completion

# Internal variables
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

.PHONY: watch
watch:
	stack build --pedantic
	stack exec site clean
	stack exec site watch

.PHONY: check
check:
	stack install hlint weeder
	weeder .
	hlint app src

.PHONY: check-genered
check-gendered:
	grep -iw "he\|him\|his" posts/*.md
