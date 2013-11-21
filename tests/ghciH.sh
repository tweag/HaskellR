#!/bin/sh
GHCi=ghci
if command -v ghcii.sh > /dev/null 2>&1
then
    GHCi=ghcii.sh
fi
$GHCi -package-db=dist/package.conf.inplace "$@"
