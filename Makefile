.PHONY: new

POST_DATE  ?= $(shell date +%Y-%m-%d)
POST_TITLE ?= $(shell echo "$(TITLE)" | sed 's/ \+/_/g; s/.*/\L&/g')
POST_PATH  ?= posts/$(POST_DATE)-$(POST_TITLE).md

new:
	[ -n "$(TITLE)" ]
	[ ! -f "$(POST_PATH)" ]
	@printf "%s\n" \
	  "---" \
	  "title: $(TITLE)" \
	  "tags:" \
	  "---" "" > "$(POST_PATH)"
	@echo CREATED: $(POST_PATH)
