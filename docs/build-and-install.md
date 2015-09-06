---
title: Building and installing the H environment
id: build-and-install
---

The H environment provides facilities for efficiently processing data
using Haskell or R code, interchangeably. H allows Haskell functions
to seamlessly call R functions and *vice versa*. It provides the
Haskell programmer with the full breadth of existing R libraries and
extensions for numerical computation and statistical analysis.

The installation procedure differs on Windows and on UNIX-like
systems. Please refer to the following sections accordingly:

* [UNIX](#INSTALL-unix) UNIX-like installation guide (Linux, Mac OS X, etc)
* [Windows](#INSTALL-win) Windows installation guide.

## <a name="INSTALL-unix"></a> Installing on unix

### Prerequisites

* A UNIX-like programming environment (Linux, OS X, BSD, etc)
* `git`
* `pkg-config`
* `R` version 3.0.2 or later (3.1 or later required for test suite).
* Either `stack` version 0.1.2 or later,
* Or, `cabal-install` version 1.18.3 or later together with Haskell
  Platform version 2013.2.0.0 or later.

### Installing via `stack`

This is the recommended installation method. To install H to
`~/.local/bin`,

    $ stack install H

See the
[project page](https://github.com/tweag/HaskellR/tree/license#developing-haskellr)
on Github for instructions to install development versions of H.

### Installing via `cabal-install`

The recommended installation method is via `stack`. The legacy method
for installing packages is documented here for completeness.

A version of `cabal-install` ships with the Haskell Platform. However,
to upgrade to the latest version,

    $ cabal install cabal-install
    $ export PATH=~/.cabal/bin:$PATH

The last line ensures that the version just installed is accessible
from your `PATH`.

`c2hs` can be installed from your distribution's package repositories
or from Hackage through `cabal-install`. If installed through
`cabal-install`, you must ensure that `c2hs` is in your `PATH` at all
times, otherwise building H will fail.

    $ cabal install c2hs
    $ export PATH=~/.cabal/bin:$PATH

To install H, change the current working directory to the folder
containing the `H.cabal` file. Then,

    $ cabal install

### Setting up H

To run H, it may be necessary to increase your stack size limit:

    $ ulimit -s unlimited

OS X does not allow removing the stack size limit completely.  You can
increase your stack size limit up to a maximum of 64MB:

    $ launchctl limit stack 67104768
    $ ulimit -s 65532

<hr />

## <a name="INSTALL-win"> </a> Installing on windows
This file explains the 32-bit installation procedure on Windows using Cygwin.

### Prerequisites

* Windows XP/7/8 installed.
* A working internet connection.
* A tarball with the H source code.

### Installation steps

In the following, we assume "joe" is the name of the user. We assume
version 3.0.2 or later for R and version 2013.2.0.0 or later for the
Haskell Platform. You will need at least R version 3.1 to run the
test suite.

### Cygwin 32-bit installation

Download `setup-x86.exe` from the [Cygwin website](http://cygwin.com/install.html) and execute it.

Accept the default settings through all the steps of the installation,
except when selecting packages where in addition to the defaults you
should select the following packages as well:

* unzip
* wget
* git (optional)
* OpenSSH (optional)

### R installation

Download and execute `R-3.0.2-win.exe` from the 
[R website](http://cran.r-project.org/bin/windows/base/).

Accept the default settings through all the steps of the installation.

### Haskell platform 32-bit installation

Download and execute HaskellPlatform-2013.2.0.0-setup.exe from the
[Haskell Platform website](http://www.haskell.org/platform/windows.html).

Accept the default settings through all the steps of the installation.

## Setting up environment variables

Open a Cygwin terminal. A shortcut in your desktop should be
available, or look for Cygwin in the start menu, or
`C:\cygwin\Cygwin.bat`.

On the command prompt, type

    $ command -v ghc
    /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/bin/ghc

    $ command -v gcc
    /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/mingw/bin/gcc

Verify that the gcc command found in the binary search `PATH` sits in
the Haskell Platform installation as above. Otherwise, it should be
added to the path with

    $ export PATH="/cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/mingw/bin/gcc":$PATH

Verify that the `cabal-install` version is at least `1.16.0.2`.

    $ cabal --version
    cabal-install version 1.16.0.2
    using version 1.16.0 of the Cabal library

Verify that the folder where `cabal-install` installs binaries is in
the path.

    $ echo $PATH | tr ":" "\n" | grep cabal/bin
    /cygdrive/c/Users/joe/AppData/Roaming/cabal/bin

Add the folder containing R 32-bit programs and libraries to the path.

    $ export PATH="/cygdrive/c/Program Files/R/R-3.0.2/bin/i386":$PATH

Verify that the LANG environment variable has the C value. This is an
optional requirement, mostly useful to get consistent error messages
and for running the test suite (which also tests error messages).

    $ echo $LANG
    C

If not, it can be set with

    $ export LANG=C
    
To make the variable settings permanent, the export commands should be
added to the file `~/.bash_profile`. You can edit the file with `vi`.

    $ vi ~/.bash_profile

Or if you prefer to use another text editor the windows path of
the file can be obtained with

    $ cygpath -w ~/.bash_profile
    C:\cygwin\home\joe\.bash_profile


### cabal-install preparation

After installing the Haskell Platform and before installing any
Haskell package, the following command is needed to update the list of
available packages that `cabal-install` can install.

    $ cabal update

## H installation

Uncompress the tarball containing the H source code.

If the file `H.tar.gz` sits in a window folder and you don't know how
to reach the folder within Cygwin, you can use the `cygpath` command.

    $ cygpath "C:\Users\joe\Desktop\H.tar.gz"
    /cygdrive/c/Users/joe/Desktop/H.tar.gz

    $ tar -xzf /cygdrive/c/Users/joe/Desktop/H.tar.gz

Build and install H.

    $ export R_HOME=$(R RHOME)
    $ echo $R_HOME
    C:\PROGRA~1\R\R-30~1.2

    $ cd H
    $ cabal install --extra-include-dirs="$R_HOME\include" \
                    --extra-lib-dirs="$R_HOME\bin\i386"

When more than one `c2hs` version is installed, sometimes it is
necessary to augment the install command with an additional parameter.
If the above command fails, try:

    $ cabal install --extra-include-dirs="$R_HOME\include" \
                    --extra-lib-dirs="$R_HOME\bin\i386"    \
                    --with-c2hs=$(cygpath -w $(command -v c2hs))

## Running H in GHCi

Open a `cmd.exe` session. This can be done by pressing the Windows key
plus `r` (Win + r), then typing `cmd` followed by the Enter key.

Place the folder containing R binaries and libraries in the path, this
time using DOS syntax.

    C:\Users\joe> set PATH=C:\PROGRA~1\R\R-30~1.2\bin\i386;%PATH%

Start H in interactive mode.

    C:\Users\joe> AppData\Roaming\cabal\bin\H --interactive
    Prelude H H H R R>

Now, Haskell expressions can be typed at the prompt.

    Prelude H H H R R> H.print =<< [r| 2+2 |]
    [1] 4



