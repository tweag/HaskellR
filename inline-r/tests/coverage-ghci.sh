#!/bin/sh
# Basically cabal support --enable-library-profiling option to 
# generate hpc reports. However our test do not use library 
# directly, but H --interactive as a result we have to introduce
# this machinery. 
[ -d .hpc ] || rm -rf .hpc
if [ "--no-recomp" != "$1" ]
then
    cabal clean
    cabal configure --enable-tests --ghc-options -fhpc
    cabal build
fi
rm *.tix > /dev/null
./dist/build/tests/tests
./dist/build/test-compile-qq/test-compile-qq
[ -d report ] || mkdir report
hpc sum --include="H-0.1.0.0:" --union --output=res.tix H.tix test-compile-qq.tix
hpc markup --include="H-0.1.0.0:" --srcdir=. res.tix --destdir=report
hpc report --include="H-0.1.0.0:" res.tix > report/summary.txt

