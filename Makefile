# Override if not in $PATH.

CABAL = cabal

all: install

.PHONY: clean doc install test

clean:
	$(CABAL) clean

install:
	$(CABAL) install

test:
	$(CABAL) install --enable-tests
