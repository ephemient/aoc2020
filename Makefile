ifndef SESSION
SESSION = $(file < $(HOME)/.aocrc)
endif
TZ := UTC+5
YEAR := 2020
DAYS := $(shell seq 1 $$(expr '(' $$(date +%s) - $$(date +%s --date=$(YEAR)-12-01) ')' / 86400 + 1) | head -n25)

all: $(DAYS:%=day%.txt)

day%.txt:
	$(if $(findstring $(@:day%.txt=%),$(DAYS)),,sleep $$(($$(date +%s --date=$(YEAR)-12-$(@:day%.txt=%)) - $$(date +%s))))
	curl -b "session=$(SESSION)" -o "day$(@:day%.txt=%).txt" "https://adventofcode.com/$(YEAR)/day/$(@:day%.txt=%)/input"

.PHONY: all
