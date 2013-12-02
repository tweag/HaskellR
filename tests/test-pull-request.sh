#!/bin/sh -eux
#
# test-pull-requet.sh <branch>
#
# Merges master and <branch> into a new branch <branch>-merged
# and then builds and runs tests.
#
cd "$(dirname "$0")/.."
git fetch origin
# bulldoze preexisting branch if any
if [[ $(git branch | grep $1-merged) ]]
then
	git checkout master
	git branch -D $1-merged || true
fi
# merge, build and test
git checkout -b $1-merged origin/$1
git merge origin/master
export RH=$(R RHOME)
cabal clean
if [[ "$(uname)" == CYGWIN* ]]
then
    cabal configure --enable-tests --extra-include-dirs=$RH/include --extra-lib-dirs=$RH/bin/i386
else
	cabal configure
fi
cabal build
cabal test
