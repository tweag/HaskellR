#!/usr/bin/env bash

function find_sandbox_package_db () {
	find '.cabal-sandbox' -type d -name '*-packages.conf.d' |
		awk '{ print } NR == 2 { exit 2 }' |
		grep .
}

GHCi=ghci
if command -v ghcii.sh > /dev/null 2>&1
then
    GHCi=ghcii.sh
fi
if [ -d '.cabal-sandbox' ] && package_db=$( find_sandbox_package_db ); then
	LANG=C ghci -fno-full-laziness -no-user-package-db -package-db="${package_db}" $GHCi_H_ARGS "$@" 2>&1
else
	LANG=C ghci -fno-full-laziness -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@" 2>&1
fi
