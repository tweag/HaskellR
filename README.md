The H environment - an R-to-Haskell interoperablity solution
============================================================

The H environment provides facilities for efficiently processing data
using Haskell or R code, interchangeably. H allows Haskell functions
to seamlessly call R functions and *vice versa*. It provides the
Haskell programmer with the full breadth of existing R libraries and
extensions for numerical computation and statistical analysis.

Installing
----------

cabal-install >= 1.16.0.2 is required to build without warnings.

This [patched
branch](https://github.com/facundominguez/c2hs/tree/bitfield-sizeAlign-fix)
of c2hs 0.16.5 is required for i386 builds on Windows:

    $ git clone -b bitfield-sizeAlign-fix git@github.com:facundominguez/c2hs.git
    $ cd c2hs
    $ cabal install

To install H, change the current working directory to the folder
containing the `H.cabal` file. Then, depending on your OS:

In Unix-like systems:

    $ cabal install

In Windows:

    $ cabal install --extra-include-dirs=$R_HOME\include \
                    --extra-lib-dirs=$R_HOME\bin\[i386|x64]

Optionally, you may add `--enable-tests` to the command line arguments
in order execute tests before installation.

In Windows you need to select which R library folder to use depending
on whether an i386 or x64 build is desired.

Setting up H in GHCi
--------------------

In Windows, make sure the file `R.dll` appears in some folder listed
in the `PATH` environment variable. In Unix-like systems, `libR.so`
should be located within reach of the dynamic linker
(`LD_LIBRARY_PATH`, `/etc/ld.so.conf`, etc).

After installing H, type the following at a command prompt:

    $ H --interactive

This will start GHCi, loading the H environment and bringing the
relevant definitions into scope. In addition, an instance of the
R interpreter will be started.

Alternatively, one can also try:

    $ ghci -ghci-script H.ghci

where H.ghci is a file colocated with the H.cabal file.

In Windows, both H and GHCi work best from the `cmd.exe` terminal, as
opposed to Mingw (but both should work).

An H primer
-----------

In an H interactive session, one has full access to both Haskell,

    H> 1 + 1
    2
    H> let it = [1, 2, 3] ++ [4, 5, 6]
    H> print it
    [1, 2, 3, 4, 5, 6]

and R, through a mechanism called *quasiquotation*,

    H> it <- [r| 1 + 1 |]
    H> H.print it
    [1] 2
    H> it <- [r| append(c(1, 2, 3), c(4, 5, 6)) |]
    H> H.print it
    [1] 1 2 3 4 5 6
    H> H.print =<< [r| R.home() |]
    [1] "/usr/lib/R"

One can mix and match both Haskell and R code, which is delimited from
Haskell code using annotated "Oxford brackets" - anything in between
`[r|` and `|]`. The text between the brackets is said to be
*quasiquoted*. It is first fed to R to be parsed, then evaluated.
Printing the resulting value can be done with the help of `H.print`.

Expressions are evaluated in the top-level R environment. As such, any
side effects such as assignments remain visible from one quasiquote to
another:

    H> H.print =<< [r| x <- 1 |]
    [1] 1
    H> H.print =<< [r| x |]
    [1] 1
    H> H.print =<< [r| x <- 2 |]
    [1] 2
    H> H.print =<< [r| x |]
    [1] 2

Quasiquotes can refer to any values bound in the GHCi environment that
are in scope, through *splicing*. In order to distinguish between
variables bound in the R environment and those bound in the GHCi
environment, H uses the following convention:

> Haskell values are referred to within an R quasiquote by appending
> `_hs` to its name.

For example:

    H> let x = 2 :: Double
    H> let y = 4 :: Double
    H> H.print =<< [r| x_hs + y_hs |]
    [1] 6

Only variables of certain types can be *spliced* in quasiquotes in
this way. H currently supports atomic numeric types such as doubles,
lists over these numeric types, but also functions over these types:

    H> let f x = return (x + 1) :: R s Double
    H> H.print =<< [r| f_hs(1) |]
    [1] 2

Currently, functions must be lifted to the `R` monad in order to be
spliceable. The `R` monad is a type constructor taking two parameters:
the first one is always `s` and refers to the state of the monad,
while the second one is the type of the result of the function when
executed.

Further reading
---------------

To find out more, please refer to the following resources, available
under the `doc/` subdirectory in the source distribution:

* "The H user guide", a manual for all users of H.
* "H internals", documenting how H works and how to extend it.
* Haddock generated API documentation for the H library.

### Building the documentation

To read these resources in your browser, type

    $ make doc

at the root of the source distribution. This will generate the
following documents:

    dist/pandoc/H-user.html
    dist/pandoc/H-ints.html
    dist/doc/html/H/index.html
