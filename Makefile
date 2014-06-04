# Override if not in $PATH.

CABAL = cabal
PANDOC = pandoc
SANDBOX = .cabal-sandbox

all: install

.PHONY: clean-dep clean install-dep install test repl

clean-dep: clean
	rm -rf $(SANDBOX) cabal.sandbox.config

clean:
	$(CABAL) clean

$(SANDBOX):
	$(CABAL) sandbox init
	$(CABAL) install --only-dep --enable-tests

install-dep: $(SANDBOX)

install: install-dep
	$(CABAL) install --enable-tests

test: install
	$(CABAL) test

# TODO: Use cabal repl instead.
repl: install
	$(SANDBOX)/bin/H --interactive -- -no-user-package-db -package-db $(SANDBOX)/*-packages.conf.d

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
