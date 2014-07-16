#!/bin/sh

. tests/sandbox-utils.sh

# NOTE: We must make both the dependencies in the sandbox and the H package
# in the dist directory available to GHC.
if package_db=`find_sandbox_package_db`; then
	LANG=C ghc -no-user-package-db -package-db="${package_db}" -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
else
	LANG=C ghc -package-db=dist/package.conf.inplace $GHC_H_ARGS "$@"
fi
