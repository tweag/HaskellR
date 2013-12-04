#!/bin/sh
[ -d .hpc ] || rm -rf .hpc
cabal clean
cabal configure --enable-tests --enable-library-profiling --ghc-options -fhpc
cabal build
rm *.tix
./dist/build/tests/tests
[ -d report ] || mkdir report
hpc report --exclude=Main H.tix > report/summary.txt
hpc markup --exclude=Main --srcdir=. H.tix --destdir=report

