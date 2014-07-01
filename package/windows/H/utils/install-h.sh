# (c) 2014, EURL Tweag
# Distributed under BSD-2 License.

#######################################################################
# This script installs H into cabal (not sandboxed).
# It should be run under cabal. This script will run
# during the installation phase but can be also launch
# manually by the user in order to install H.
#######################################################################

# Carriage return, we need it to remove redundant returns
# from the PATHs that are read from the registry
# cr=$(echo -e "\r")
# cn=$(echo -e "\n")


# Will fix a H sources directory, for now
echo "Initialize H"

HHOME="$(cygpath "${HRoot}")"
HSOURCE="${HHOME}/sources"			# H Sources
HUTILS="${HHOME}/utils"				# H Utilities

echo "H Directories:"
echo "	Home: $HHOME"
echo "	Sources: $HSOURCE"
echo "	Utilities: $HUTILS" 

echo "Initialize R"
RPATH="$(cygpath "${RRoot}")"

echo ".. exporting R path to the environment: $RPATH/bin"

export PATH="$RPATH/bin:$PATH"
R_HOME="$(R RHOME)"
echo "R Directories:"
echo "	Base: $RPATH"
echo "	R_HOME: $R_HOME"

# Prepare haskell platform and cabal.
cabal update
cabal install c2hs
cabal install cabal-install
echo "Initialize Haskell Platform."
echo "Add user's cabal/bin folder to the PATH."
export PATH="$(cygpath "${APPDATA}")/cabal/bin:$PATH"

echo "Haskell Platform Directories:"
echo "	Binary Path: ${APPDATA}/cabal/bin"

# now we need to check cabal version
echo "Cabal version is: $(cabal --numeric-version)"
echo "..no need to update cabal-install: skipping"

echo "Build H"
BUILDDIR="$(mktemp -d)"
echo "Created a temporary build directory: $BUILDDIR"

echo "Installing dependencies"
cp -r "${HSOURCE}" "${BUILDDIR}"

pushd "${BUILDDIR}"
echo "Compiling H"
cabal install \
		--extra-include-dirs="$R_HOME/include" \
		--extra-lib-dirs="$R_HOME/bin/i386" \
		--with-c2hs=$(cygpath -w $(command -v c2hs))
echo "..Done"
popd
rm -rf "$BUILDDIR"
