---
title: Building and installing HaskellR
id: build-and-install
---

## Prerequesites

* `git` and `pkg-config` installed.
* Either `stack` version 0.1.2 or later,
* Or, `cabal-install` version 1.18.3 or later together with Haskell
  Platform version 2013.2.0.0 or later.

If you are not using stack's Docker support detailed below, you will
furthermore need:

* `R` version 3.0.2 or later (3.1 or later required for test suite).
* (Optional) ZeroMQ 3.0 or later.
* (Optional) Jupyter/IPython version 3.2 or later.

TODO installation instructions for Windows.

## From Hackage

### Installing H

The H interactive environment can be copied to `~/.local/bin` using
[stack][stack] (recommended):

```
$ stack build --copy-bins H
```

If stack complains about not being able to find `inline-r`, add it to
your global stack config, for example like so:

```
flags: {}
packages: []
extra-deps:
  - inline-r-0.7.0.0
resolver: lts-3.4
```

You can then run `H` with

```
stack exec H
```

Alternatively, you can use [cabal-install][cabal-install], which will
copy the binary to `~/.cabal/bin` by default:

```
$ cabal install H
```

Make sure to include the target directories in your `PATH`. On UNIX
systems:

```
$ export PATH=~/.cabal/bin:$PATH
```

[stack]: https://github.com/commercialhaskell/stack
[cabal-install]: https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package

### Installing Jupyter/IHaskell support for inline-r

H is a very basic interactive environment. It is easy to install. If
you would like a more featureful environment, HaskellR includes a plugin
for Jupyter's [IHaskell][ihaskell] kernel. Since the latter depends on
a number of system libraries that may or may not be installed using
exactly the right configuration by your distribution, on Linux it is
recommended to use stack's Docker support to get reliable installs,
explained in the "From Github" section below.

[ihaskell]: https://github.com/gibiansky/IHaskell

## From Github

An alternative to using the release versions on Hackage is to check
out the source code from the Github repository directly. You will need
to use this method if you want to take advantage of stack's Docker
support to get reliable IHaskell builds.

```
$ git clone http://github.com/tweag/HaskellR
$ cd HaskellR
$ stack --docker build
$ stack --docker exec ihaskell install
```

Now, you can open a new Jupyter notebook in your browser using

```
$ stack --docker exec ipython notebook
```

or pop into an H REPL with

```
$ stack --docker exec H
```

Read the [tutorial][tutorial] to learn more about using inline-r and
IHaskell together.

[tutorial]: https://github.com/tweag/HaskellR/blob/master/IHaskell/examples/tutorial-ihaskell-inline-r.ipynb
