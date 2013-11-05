The H compiler
==============

An R-to-Haskell translator and interoperability solution.


Running H
---------

In order to run H you need to provide `R_HOME` environment 
variable. This can be done either by disto-specific settings 
or by calling H as:

    R_HOME=`pkg-config --variable=rhome libR` H


