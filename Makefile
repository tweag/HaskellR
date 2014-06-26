all: test

ci: depclean test doc

PKG_DESCRIPTION = H.cabal
DEP = .cabal-sandbox
CONFIGURE = dist/setup-config

ifndef PREFIX
	PREFIX = /usr/local
endif

.PHONY: ci install dep configure build test run doc doc-haddock coverage clean depclean

# NOTE: This requires `alex`, `happy`, and `hscolour` to be already installed.
install:
	cabal install vendor/c2hs --global --ignore-sandbox --prefix=$(PREFIX)
	cabal install \
	  --enable-documentation \
	  --enable-tests \
	  --global \
	  --ignore-sandbox \
	  --haddock-hyperlink-source \
	  --prefix=$(PREFIX) \
	  --reorder-goals \
	  --run-tests

$(DEP): $(PKG_DESCRIPTION)
	[ -e $(DEP) ] || ( \
	  cabal sandbox init && \
	  cabal install alex happy hscolour && \
	  cabal install vendor/c2hs )
	cabal install \
	  --dependencies-only \
	  --enable-documentation \
	  --enable-tests \
	  --haddock-hyperlink-source \
	  --reorder-goals

dep: $(DEP)

$(CONFIGURE): $(DEP)
	cabal configure --enable-tests

configure: $(CONFIGURE)

build: $(CONFIGURE)
	cabal build

test: $(CONFIGURE)
	cabal test --show-details=streaming

# NOTE: We must make available to H both the dependencies in the sandbox
# and the H package in the build directory.
run: $(CONFIGURE)
	cabal run -- --interactive -- -package-db .cabal-sandbox/*-packages.conf.d -package-db dist/package.conf.inplace

dist/pandoc/H-ints.html: doc/H-ints.md doc/pandoc.css
	mkdir -p dist/pandoc
	cp doc/pandoc.css dist/pandoc
	pandoc -f markdown --mathjax -s -S --toc -c pandoc.css $< -o $@

dist/pandoc/H-user.html: doc/H-user.md doc/pandoc.css
	mkdir -p dist/pandoc
	cp doc/pandoc.css dist/pandoc
	pandoc -f markdown --mathjax -s -S --toc -c pandoc.css $< -o $@

# NOTE: Passing `--ghc-options=-optP-P` is a workaround for an issue with
# GHC 7.8.2/Haddock 2.14.2 on OS X.
# https://ghc.haskell.org/trac/ghc/ticket/9174
doc-haddock: $(CONFIGURE)
	cabal haddock --ghc-options=-optP-P --hyperlink-source

doc: doc-haddock dist/pandoc/H-ints.html dist/pandoc/H-user.html

# TODO: Investigate coverage.
coverage:
	sh tests/coverage-ghci.sh $(ARGS)

clean:
	rm -rf dist

depclean:
	rm -rf .cabal-sandbox cabal.sandbox.config dist
