The H compiler
==============

An R-to-Haskell translator and interoperability solution.


Running H
---------

In order to run H you need to provide `R_HOME` environment 
variable. This can be done either by disto-specific settings 
or by calling H as:

    R_HOME=`pkg-config --variable=rhome libR` H

If `R_HOME` is not provided then H will inspect output of
R RHOME in runtime.

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

