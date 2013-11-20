The H compiler
==============

An R-to-Haskell translator and interoperability solution.

Installing
----------

cabal-install >= 1.16.0.2 is required to build without
warnings.

Unix

    cabal install

Windows

    cabal install --extra-include-dirs=$R_HOME\include \
                  --extra-lib-dirs=$R_HOME\bin\x64

Running H
---------

In order to run H you need to provide `R_HOME` environment 
variable. This can be done either by disto-specific settings 
or by calling H as:

    R_HOME=`pkg-config --variable=rhome libR` H

If `R_HOME` is not provided then H will inspect output of
R -e "cat(R.home())" at runtime.

Running GHCi
------------

To use H in ghci you need to provide `R_HOME` variable,
see Running H. To start ghci use `ghci -no-ghci-sandbox`.
Then load R runtime:

   :m + Language.R.Interpreter

   > runInterpreter

Starting from this moment you may use H function.
Note that you can't even use pure functions without the 
R runtime being loaded first.

