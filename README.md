The H environment - an R-to-Haskell interoperablity solution
============================================================

The H project provides an interpreted translation and a
compiled translation. Since the compiled translation is in
flux so it will not be discussed further.

Quasiquotation only works in GHCi at the moment but should
work even in Haskell source files in the future.


Installing
----------

cabal-install >= 1.16.0.2 is required to build without
warnings.

This [patched branch](https://github.com/facundominguez/c2hs/tree/bitfield-sizeAlign-fix)
of c2hs 0.16.5 is required for i386 builds on Windows:

    git clone -b bitfield-sizeAlign-fix git@github.com:facundominguez/c2hs.git
	cd c2hs
	cabal install

To install H, change the current working directory to the
folder containing the H.cabal file. Then, depending on your
OS:

In Unix-like systems

    cabal install

In Windows

    cabal install --extra-include-dirs=$R_HOME\include \
                  --extra-lib-dirs=$R_HOME\bin\[i386|x64]


Optionally, you may add --enable-tests to the command line
arguments in order execute tests before installation.

In Windows you need to select which R library folder to use
depending on whether an i386 or x64 build is desired.


Setting up H in GHCi
--------------------

In Windows, make sure the file R.dll appears in some folder
listed in the PATH environment variable. In Unix-like
systems, libR.so should be located within reach of the
dynamic linker (LD_LIBRARY_PATH, /etc/ld.so.conf, etc).

After installing H, type at the prompt:

    $ H --interactive

This will start ghci loading the H environment and bringing
the relevant definitions into scope. In addition, an instance
of the R interpreter will be started in the background.

Alternatively, the following should work:

    $ ghci -ghci-script H.ghci

where H.ghci is a file colocated with the H.cabal file.

When in Windows, both H and ghci work best with the cmd
terminal.


Using H in GHCi
---------------

Now expressions can be evaluated with the r quasi-quoter.

    H> H.print =<< [r| 1 + 1 |]
    [1] 2
    H> H.print =<< [r| R.home() |]
    [1] "/usr/lib/R"

The r quasi-quoter will pass expressions to the R
interpreter and return the result that is printed.

Because expressions are passed to the R interpreter, the
effect of assignments is remembered.

    H> H.print =<< [r| x <- 1 |]
    [1] 1
    H> H.print =<< [r| x |]
    [1] 1

Quasiquotes can refer to values bound in Haskell in the
lexical scope surrounding it, through _splicing_.

Splicing Haskell values is denoted by appending the "_hs" to
the name of the Haskell variable that ought to be spliced.

    H> let x = 2 :: Double
    H> H.print =<< [r| x_hs + x_hs |]
    [1] 4

And we mean even functions can be passed:

    H> let f = (\x -> return (x + 1)) :: Double -> IO Double
    H> H.print =<< [r| f_hs(1) |]
    [1] 2
