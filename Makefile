# Override if not in $PATH.

CABAL = cabal
PANDOC = pandoc

all: install

.PHONY: clean install test

clean:
	$(CABAL) clean

install:
	$(CABAL) install

test:
	$(CABAL) install --enable-tests

doc-internals:

.PHONY: doc doc-internals doc-haddock

dist/pandoc/H-ints.html: doc/H-ints.md doc/pandoc.css
	mkdir -p dist/pandoc
	cp doc/pandoc.css dist/pandoc
	$(PANDOC) -f markdown --mathjax -s -S --toc -c pandoc.css $< -o $@

dist/pandoc/H-user.html: doc/H-user.md doc/pandoc.css
	mkdir -p dist/pandoc
	cp doc/pandoc.css dist/pandoc
	$(PANDOC) -f markdown --mathjax -s -S --toc -c pandoc.css $< -o $@

doc-internals: dist/pandoc/H-ints.html
doc-users-guide: dist/pandoc/H-user.html

doc-haddock: install
	$(CABAL) haddock

doc: doc-haddock doc-internals doc-users-guide

.PHONY: coverage
coverage:
	sh tests/coverage-ghci.sh $(ARGS)
