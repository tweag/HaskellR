#!/bin/sh

# Find the sandbox package DB and echo its path to stdout.
# Fail if there is no sandbox, or if there is more than one package DB
# within the sandbox.
find_sandbox_package_db () {
	if ! [ -d '.cabal-sandbox' ]; then
		return 1
	fi
	find '.cabal-sandbox' -maxdepth 1 -type d -name '*-packages.conf.d' |
		awk '{ print } NR == 2 { exit 2 }' |
		grep .
}
