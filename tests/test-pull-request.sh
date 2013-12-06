#!/bin/bash -eux
#
# test-pull-requet.sh [-s <sandbox>] <branch>
#
# Merges master and <branch> into a new branch <branch>-merged
# and then builds and runs tests.
#
# If -s <sandbox> is specified cabal-dev will be used to
# configure the build using the provided sandbox folder.
#

if [[ "-s" == "$1" ]]
then
    CABAL="cabal-dev -s $2"
    BRANCH=$3
    export GHCi_H_ARGS="-package-db=$2/packages-$(ghc --numeric-version).conf"
else
    CABAL=cabal
    BRANCH=$1
fi

if ghc --info | grep ArchX86_64 > /dev/null
then
    ARCH=x64
else
    ARCH=i386
fi

git fetch origin
# bulldoze preexisting branch if any
if [[ $(git branch | grep $BRANCH-merged) ]]
then
	git checkout master
	git branch -D $BRANCH-merged || true
fi
# merge, build and test
git checkout -b $BRANCH-merged origin/$BRANCH
git merge origin/master
export RH=$(R RHOME)
cabal clean
if [[ "$(uname)" == CYGWIN* ]]
then
    $CABAL configure --with-c2hs=$(cygpath -w $(command -v c2hs)) --enable-tests --extra-include-dirs=$RH\\include --extra-lib-dirs=$RH\\bin\\$ARCH
else
	$CABAL configure
fi
cabal build
cabal test
