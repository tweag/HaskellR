#!/bin/sh
# Basically cabal support --enable-library-profiling option to 
# generate hpc reports. However our test do not use library 
# directly, but H --interactive as a result we have to introduce
# this machinery. 
[ -d .hpc ] || rm -rf .hpc
cabal clean
cabal configure --enable-tests --ghc-options -fhpc
cabal build
rm *.tix > /dev/null
./dist/build/tests/tests
[ -d report ] || mkdir report
hpc markup --include="H-0.1.0.0:" --srcdir=. H.tix --destdir=report
hpc report --include="H-0.1.0.0:" H.tix > report/summary.txt

