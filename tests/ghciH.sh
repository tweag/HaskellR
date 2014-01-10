#!/bin/sh
GHCi=ghci
if command -v ghcii.sh > /dev/null 2>&1
then
    GHCi=ghcii.sh
fi
LANG=C $GHCi -fno-full-laziness -package-db=dist/package.conf.inplace $GHCi_H_ARGS "$@" 2>&1
