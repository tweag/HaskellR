# Override if not in $PATH.

CABAL = cabal
PANDOC = pandoc
SANDBOX = .cabal-sandbox

all: ci

.PHONY: ci delete sandbox clean configure build test run

ci: delete configure test doc

delete: clean
	$(CABAL) sandbox delete

$(SANDBOX):
	$(CABAL) sandbox init
	$(CABAL) install alex happy
	$(CABAL) install vendor/c2hs
	$(CABAL) install --only-dependencies --enable-tests

sandbox: $(SANDBOX)

clean:
	$(CABAL) clean

configure: $(SANDBOX)
	$(CABAL) configure --enable-tests

build: $(SANDBOX)
	$(CABAL) build

test: $(SANDBOX)
	$(CABAL) test

# NOTE: We must make both the dependencies in the sandbox and the H package
# in the dist directory available to the H executable.
run: $(SANDBOX)
	$(CABAL) run -- --interactive -- -package-db .cabal-sandbox/*-packages.conf.d -package-db dist/package.conf.inplace

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

doc-haddock: configure
	$(CABAL) haddock

doc: doc-haddock doc-internals doc-users-guide

.PHONY: coverage

coverage:
	sh tests/coverage-ghci.sh $(ARGS)
