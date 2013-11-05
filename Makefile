# Override if not in $PATH.

CABAL = cabal
PANDOC = pandoc

all: install

.PHONY: clean doc install test

clean:
	$(CABAL) clean

install:
	$(CABAL) install

test:
	$(CABAL) install --enable-tests

doc:
	$(CABAL) haddock
	$(PANDOC) -s doc/H-ints.md -o dist/pandoc/H.ints.html
