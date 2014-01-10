#!/bin/sh
LANG=C ghc -fno-full-laziness -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
