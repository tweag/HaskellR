#!/bin/sh

. tests/sandbox-utils.sh

GHCi=ghci
if command -v ghcii.sh > /dev/null 2>&1
then
    GHCi=ghcii.sh
fi

# NOTE: We must make both the dependencies in the sandbox and the H package
# in the dist directory available to GHCi.
if package_db=`find_sandbox_package_db`; then
	LANG=C ${GHCi} -fno-full-laziness -no-user-package-db -package-db="${package_db}" -package-db=dist/package.conf.inplace $GHCi_H_ARGS "$@" 2>&1
else
	LANG=C ${GHCi} -fno-full-laziness -package-db=dist/package.conf.inplace $GHCi_H_ARGS "$@" 2>&1
fi
