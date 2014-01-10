#!/bin/sh
LANG=C ghc -fno-full-laziness -O2 -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
