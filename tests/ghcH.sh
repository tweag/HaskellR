#!/bin/sh
LANG=C ghc -fno-cse -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
