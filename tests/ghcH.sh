#!/usr/bin/env bash

function find_sandbox_package_db () {
	find '.cabal-sandbox' -type d -name '*-packages.conf.d' |
		awk '{ print } NR == 2 { exit 2 }' |
		grep .
}

if [ -d '.cabal-sandbox' ] && package_db=$( find_sandbox_package_db ); then
	LANG=C ghc -fno-full-laziness -no-user-package-db -package-db="${package_db}" $GHC_H_ARGS "$@"
else
	LANG=C ghc -fno-full-laziness -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
fi
