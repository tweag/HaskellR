---
title: Using H
id: using-h
---

H is a library, as well as a command. The library can be used from
Haskell source files as well as from GHCi. The command is a wrapper
script that fires up a GHCi session set up just the right way for
interacting with R.

Setting up H in GHCi
--------------------

In Windows, make sure the file `R.dll` appears in some folder listed
in the `PATH` environment variable. In Unix-like systems, `libR.so`
should be located within reach of the dynamic linker
(`LD_LIBRARY_PATH`, `/etc/ld.so.conf`, etc).

After installing H, type the following at a command prompt:

    $ H

This will start GHCi, loading the H environment and bringing the
relevant definitions into scope. In addition, an instance of the
R interpreter will be started.

Alternatively, one can also try:

    $ ghci -ghci-script H.ghci

where `H.ghci` is a file colocated with the `H.cabal` file. (NB: for
security reasons, you must ensure that `H.ghci` is not world
writeable.)

In Windows, both H and GHCi work best from the `cmd.exe` terminal, as
opposed to MinGW (both ought work, but MinGW currently triggers bug
[#7056](https://ghc.haskell.org/trac/ghc/ticket/7056) in GHC).

An H primer
-----------

In an H interactive session, one has full access to both Haskell,

    H> 1 + 1
    2
    H> let it = [1, 2, 3] ++ [4, 5, 6]
    H> print it
    [1, 2, 3, 4, 5, 6]

/and R, through a mechanism called *quasiquotation* (more details could be found on [Haskel Wiki](https://wiki.haskell.org/Quasiquotation)),

    H> it <- [r| 1 + 1 |]
    H> H.print it
    [1] 2
    H> it <- [r| append(c(1, 2, 3), c(4, 5, 6)) |]
    H> H.print it
    [1] 1 2 3 4 5 6
    H> H.printQuote [r| R.home() |]
    [1] "/usr/lib/R"

One can mix and match both Haskell and R code, which is delimited from
Haskell code using annotated "Oxford brackets" - anything in between
`[r|` and `|]`. The text between the brackets is said to be
*quasiquoted*. It is first fed to R to be parsed, then evaluated.
Printing the resulting value can be done with the help of `H.print` or
`H.printQuote`. We have that

    printQuote mx = do { x <- mx; H.print x }

Expressions are evaluated in the top-level R environment. As such, any
side effects such as assignments remain visible from one quasiquote to
another:

    H> H.printQuote [r| x <- 1 |]
    [1] 1
    H> H.printQuote [r| x |]
    [1] 1
    H> H.printQuote [r| x <- 2 |]
    [1] 2
    H> H.printQuote [r| x |]
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
    H> H.printQuote [r| x_hs + y_hs |]
    [1] 6

Only variables of certain types can be *spliced* in quasiquotes in
this way. H currently supports atomic numeric types such as doubles,
lists over these numeric types, but also functions over these types:

    H> let f x = return (x + 1) :: R s Double
    H> H.printQuote [r| f_hs(1) |]
    [1] 2

Currently, functions must be lifted to the `R` monad in order to be
spliceable. The `R` monad is a type constructor taking two parameters:
the first one is always `s` and refers to the state of the monad,
while the second one is the type of the result of the function when
executed.

Running examples
----------------

Some interactive examples of using H are located in the folders:

    examples/nls
    examples/nls2

The following commands can be used to run these examples:

    $ cd examples/<example-name>
    $ H -- -ghci-script <example-name>.H