#!/bin/sh -d
[ -d .hpc ] || rm -rf .hpc
cabal clean
cabal configure --enable-tests --ghc-options -fhpc
cabal build
rm *.tix > /dev/null
./dist/build/tests/tests
[ -d report ] || mkdir report
hpc markup --include="H-0.1.0.0:" --srcdir=. H.tix --destdir=report
hpc report --include="H-0.1.0.0:" H.tix > report/summary.txt

