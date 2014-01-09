#!/bin/sh
LANG=C ghc -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
