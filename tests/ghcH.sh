#!/bin/sh
LANG=C ghc -O0 -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
