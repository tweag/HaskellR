---
title: Building and installing HaskellR
id: build-and-install
---

*Note: if you prefer to use your system's globally installed packages
and to install from Hackage, see the section below. If you run into
any issues see the [support](../support.html) page.*

The easiest way to get started on Linux or OS X is using
[Stack][stack] and its built-in [Nix][nix] support:

~~~
$ git clone http://github.com/tweag/HaskellR
$ cd HaskellR
$ git tag -l                        # list releases
$ git checkout v<latest-release>    # skip if you want the master branch
$ stack --nix build
~~~

You can make passing `--nix` to all Stack commands implicit by adding

~~~
nix:
  enable: true
~~~

to your `stack.yaml`. Alternatively, use Docker, which like Nix
obviates the need to install any dependencies globally on your system:

~~~
$ stack --docker build
~~~

[stack]: http://haskellstack.org
[nix]: http://nixos.org/nix

## Non sandboxed builds from Hackage

If you'd rather use your system's globally installed packages, here
are the system dependencies we rely on:

* [pkg-config][pkg-config],
* `R` version 3.0.2 or later (3.1 or later required for test suite).
* (Optional) ZeroMQ 3.0 or later.
* (Optional) Jupyter/IPython version 3.2 or later.

[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/

**OS X users:** use Homebrew to install dependencies, e.g.

~~~
$ brew update
$ brew tap homebrew/science
$ brew install r zeromq
$ pip install ipython    # Only needed for IHaskell support
~~~

**Windows users:** Only `inline-r` and `H` are supported (no Jupyter
support yet). After installing R somewhere on your system, you'll need
to pass additional flags when building and installing, as in the
following example:

~~~
$ stack build H --extra-lib-dirs=C:\R\bin\i386 --extra-include-dirs=C:\R\include
~~~

Once the system dependencies are installed, you can install H or the
Jupyter kernel as below.

## Installing H

You can launch the H interactive environment locally:

~~~
$ stack [--nix|--docker] exec H
~~~

But you can also install it user-globally to `~/.local/bin`:

~~~
$ stack build --copy-bins H
~~~

Make sure to include the installation directory in your `PATH`. On
UNIX systems:

~~~
$ export PATH=~/.local/bin:$PATH
~~~

## Installing Jupyter/IHaskell support for inline-r

H is a very basic interactive environment. It is easy to install. If
you would like a more featureful environment, HaskellR includes
a plugin for Jupyter's [IHaskell][ihaskell] kernel. Since the latter
depends on a number of system libraries that may or may not be
installed using exactly the right configuration by your distribution,
on Linux and OS X it is recommended to use Stack's Nix or Docker
support to get reliable installs.

[ihaskell]: https://github.com/gibiansky/IHaskell

~~~
$ stack [--docker|--nix] exec ihaskell install
~~~

Now, you can open a new Jupyter notebook in your browser using

~~~
$ stack [--docker|--nix] exec jupyter notebook
~~~

After launching the Jupyter notebook server you can visit

~~~
http://localhost:8888/notebooks/ihaskell-inline-r/examples/tutorial-ihaskell-inline-r.ipynb
~~~

in your browser for an interactive tutorial, which is available in
static form [here][tutorial].

[tutorial]: https://github.com/tweag/HaskellR/blob/master/ihaskell-inline-r/examples/tutorial-ihaskell-inline-r.ipynb
