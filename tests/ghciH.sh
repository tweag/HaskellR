#!/bin/sh
GHCi=ghci
if [[ -x $(which ghcii.sh) ]]
then
    GHCi=ghcii.sh
fi
$GHCi -package-db=dist/package.conf.inplace "$@"
