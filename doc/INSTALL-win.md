This file explains how to install a 32 bit version of H on windows
using Cygwin.

Prerequisites
=============

* Windows 7 installed
* Working internet connection
* A tarball with the H source code

Installation steps
==================

Cygwin 32-bit installation
--------------------------

Download setup-x86.exe from the
[cygwin website](http://cygwin.com/install.html) and execute it.

Accept the default settings through all the steps of the installation,
except when selecting packages where in addition to the defaults you
should select the following packages as well:

  * git
  * OpenSSH
  * unzip
  * wget


R installation
--------------

Download and execute R-3.0.2-win.exe from the 
[R website](http://cran.r-project.org/bin/windows/base/).

Accept the default settings through all the steps of the installation.


Haskell platform 32-bit installation
------------------------------------

Download and execute HaskellPlatform-2013.2.0.0-setup.exe from the
[Haskell Platform website](http://www.haskell.org/platform/windows.html).

Accept the default settings through all the steps of the installation.


Setting up environment variables
--------------------------------

Open a cygwing terminal. A shortcut in your desktop should be
 available, or look for cygwin in the start menu, or
`C:\cygwin\Cygwin.bat`.

On the command prompt type

    $ command -v ghc
    /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/bin/ghc

    $ command -v gcc
    /cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/mingw/bin/gcc

Verify that the gcc command found in the binary search PATH sits in
the Haskell Platform installation as above. Otherwise, it could be
 added to the path with

    $ export PATH="/cygdrive/c/Program Files (x86)/Haskell Platform/2013.2.0.0/mingw/bin/gcc":$PATH

Verify that cabal-install version is 1.16.0.2.

    $ cabal --version
    cabal-install version 1.16.0.2
    using version 1.16.0 of the Cabal library

Verify that the folder where cabal-install installs binaries is in the path. 

    $ echo $PATH | tr ":" "\n" | grep cabal/bin
    /cygdrive/c/Users/facundo/AppData/Roaming/cabal/bin

Add the folder containing R 32-bit programs and libraries to the path.

    $ export PATH="/cygdrive/c/Program Files/R/R-3.0.2/bin/i386":$PATH

Verify that the LANG environment variable has the C value.

    $ echo $LANG
    C

If not, it can be set with

    $ export LANG=C
    

To make the variable settings permanent, the export commands should
be added to the file `~/.bash_profile`. You can edit the file with vi.

    $ vi ~/.bash_profile

Or if you prefer to use another text editor the windows path of
the file can be obtained with

    $ cygpath -w ~/.bash_profile
    C:\cygwin\home\facundo\.bash_profile


cabal-install preparation
-------------------------

After installing the Haskell Platform and before installing any Haskell
package, the following command is needed to update the list of available
packages that cabal-install can install.

    $ cabal update


c2hs installation
-----------------

This [patched branch](https://github.com/facundominguez/c2hs/tree/bitfield-sizeAlign-fix)
of c2hs 0.16.5 is required for i386 builds on Windows. The branch can
be obtained either using git or downloading a zip file.

The git way:

    $ git clone -b bitfield-sizeAlign-fix https://github.com/facundominguez/c2hs.git

The zip way

    $ wget https://github.com/facundominguez/c2hs/archive/bitfield-sizeAlign-fix.zip
    $ unzip c2hs-bitfield-sizeAlign-fix.zip
    $ mv c2hs-bitfield-sizeAlign-fix c2hs

Then the package can be installed as follows

    $ cd c2hs
    $ cabal install


H installation
--------------

Uncompress the tarball containing the H source code.

If the file `H.tar.gz` sits in a window folder and you don't know how
to reach the folder within cygwin, you can use the cygpath command.

    $ cygpath "C:\Users\facundo\Desktop\H.tar.gz"
    /cygdrive/c/Users/facundo/Desktop/H.tar.gz

    $ tar -xzf /cygdrive/c/Users/facundo/Desktop/H.tar.gz

Build and install H.

    $ export R_HOME=$(R RHOME)
    $ echo $R_HOME
    C:\PROGRA~1\R\R-30~1.2

    $ cd H
    $ cabal install --extra-include-dirs="$R_HOME\include" \
                    --extra-lib-dirs="$R_HOME\bin\i386"

When more than one c2hs version is installed, sometimes it is
necessary to augment the install command with an additional parameter.
If the above command fails, try:

    $ cabal install --extra-include-dirs="$R_HOME\include" \
                    --extra-lib-dirs="$R_HOME\bin\i386"    \
                    --with-c2hs=$(cygpath -w $(command -v c2hs))


Running H in GHCi
-----------------

Open a cmd terminal. This can be done by pressing the Windows key plus
`r` (Win + r), then typing `cmd` followed by the Enter key.

Place the folder containing R binaries and libraries in the path, this
time using DOS syntax.

    C:\Users\facundo> set PATH=C:\PROGRA~1\R\R-30~1.2\bin\i386;%PATH%

Start H in interactive mode.

    C:\Users\facundo> AppData\Roaming\cabal\bin\H --interactive
    Prelude H H H R R>

Now, Haskell expressions can be typed at the prompt.

    Prelude H H H R R> H.print =<< [r| 2+2 |]
    [1] 4