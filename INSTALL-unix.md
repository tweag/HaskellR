The H Installation Guide - for UNIX
===================================

Prerequisites
-------------

* A UNIX-like programming environment (Linux, Mac OS X, *BSD, etc)
* Git
* `pkg-config`
* R version 3.0.2 or later.
* `cabal-install` version 1.16.0.2 or later.
* Haskell Platform version 2013.2.0.0 or later.
* `c2hs` version 0.17.3 or later.

Installing `cabal-install`
--------------------------

A version of `cabal-install` ships with the Haskell Platform. However,
to upgrade to the latest version,

    $ cabal install cabal-install
    $ export PATH=~/.cabal/bin:$PATH

The last line ensures that the version just installed is accessible
from your `PATH`.

Installing `c2hs`
-----------------

`c2hs` can be installed from your distribution's package repositories
or from Hackage through `cabal-install`. If installed through
`cabal-install`, you must ensure that `c2hs` is in your `PATH` at all
times, otherwise building H will fail.

    $ cabal install c2hs
    $ export PATH=~/.cabal/bin:$PATH

Installing H
------------

To install H, change the current working directory to the folder
containing the `H.cabal` file. Then,

    $ cabal install

Optionally, you may add `--enable-tests` to the command line arguments
in order execute tests before installation.
