The H Hacking Guide
===================

Preparation
-----------

To prepare our patched version of `c2hs`:

    $ git submodule update --init

For system administrators
-------------------------

To install H in `/usr/local`, having previously installed `alex`, `happy`, and
`hscolour`:

    $ make install

To install H in a custom location:

    $ make install PREFIX=/foo/bar

For developers
--------------

To install all dependencies in a new Cabal sandbox, build H, and run all tests:

    $ make

H uses the default sandbox location, `.cabal-sandbox`, and the default build
directory location, `dist`.

To delete the build directory:

    $ make clean

To delete the sandbox and the build directory:

    $ make depclean

H uses `pandoc` to build certain parts of its documentation.  If `pandoc` is
not available, building documentation will fail.

The continuous integration target deletes the sandbox and the build directory,
installs all dependencies in a new sandbox, builds H, runs all tests, and
builds all documentation:

    $ make ci

To create the sandbox and install all dependencies in it:

    $ make dep

All of the following targets require the sandbox, and automatically create it
if needed.

To configure the build:

    $ make configure

All of the following targets require the build to be configured, and
automatically configure it if needed.

To build H without installing it:

    $ make build

To run all tests without installing H:

    $ make test

To run an interactive H session without installing H:

    $ make run

To build all documentation without installing H:

    $ make doc
