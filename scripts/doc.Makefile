SOURCES=$(shell find content/ -name '*.md')
HTML_FILES := $(patsubst %.md,%.html,$(SOURCES))
HTML_TARGETS := $(addprefix _site/, $(HTML_FILES:content/%=%))
TEXT_FILES := $(patsubst %.md,%.txt,$(SOURCES))
TEXT_TARGETS := $(addprefix _index/, $(TEXT_FILES:content/%=%))

TEMPLATE?=scripts/template.html

.PHONY: all
all: $(HTML_TARGETS) \
	man \
	_site/favicon.ico \
	_site/robots.txt _site/humans.txt _site/.well-known/security.txt \
	_site/documentation/clis/curiosity.7.html \
	_site/documentation/clis/cty.1.html \
	_site/documentation/search.html \
	_site/static/indexes/content.st \
	_site/static/indexes/stork.css \
	_site/documentation/asciinema.html \
	_site/static/asciinema/demo.cast \
	_site/static/asciinema/asciinema-player-3.0.1.css \
	_site/static/asciinema/asciinema-player-3.0.1.min.js

man: curiosity.7.gz cty.1.gz

_site/documentation/search.html: content/documentation/search.html
	mkdir -p $(dir $@)
	cp $< $@

_site/documentation/asciinema.html: content/documentation/asciinema.html
	mkdir -p $(dir $@)
	cp $< $@

_site/%.html: content/%.md $(TEMPLATE)
	mkdir -p $(dir $@)
	pandoc --standalone \
		--toc \
		--section-divs \
		--to html5 \
		--template $(TEMPLATE) \
		--output $@ \
		$<

_site/documentation/clis/%.1.html: %.1
	mkdir -p $(dir $@)
	mandoc -T html $< > $@

_site/documentation/clis/%.7.html: %.7
	mkdir -p $(dir $@)
	mandoc -T html $< > $@

%.1: man/%.1.md
	pandoc --standalone --to man $< -o $@

%.7: man/%.7.md
	pandoc --standalone --to man $< -o $@

%.1.gz: %.1
	gzip --keep $<

%.7.gz: %.7
	gzip --keep $<

_site/favicon.ico: content/favicon.ico
	cp $< $@

_site/robots.txt: content/robots.txt
	cp $< $@

_site/humans.txt: content/humans.txt
	cp $< $@

_site/.well-known/security.txt: content/.well-known/security.txt
	mkdir -p $(dir $@)
	cp $< $@

_site/static/indexes/content.st: $(TEXT_TARGETS) scripts/stork.toml
	mkdir -p $(dir $@)
	stork --version
	stork build --input scripts/stork.toml --output $@

_site/static/indexes/stork.css: scripts/stork.css
	mkdir -p $(dir $@)
	cp $< $@

_index/%.txt: content/%.md
	mkdir -p $(dir $@)
	pandoc \
		--to plain \
		--output $@ \
		$<

_site/static/asciinema/%.cast: scripts/%.cast
	mkdir -p $(dir $@)
	cp $< $@

_site/static/asciinema/%.css: scripts/%.css
	mkdir -p $(dir $@)
	cp $< $@

_site/static/asciinema/%.js: scripts/%.js
	mkdir -p $(dir $@)
	cp $< $@

entr:
	find content/ -name '*.md' \
		| entr -c bash -c 'make -f scripts/doc.Makefile TEMPLATE=scripts/template-public.html'
