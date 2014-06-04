The H Hacking Guide
===================

H supports building in a Cabal sandbox.  To set up the sandbox,

    $ git submodule update --init
    $ make sandbox

To tear down the sandbox,

    $ make delete

Other top-level `make` commands simply invoke the corresponding `cabal`
commands, with the addition that `configure` always uses `--enable-tests`.

After `configure`, the expected workflow includes any number of `build`,
`test`, `run`, or `doc` commands:

    $ make build
    $ make test
    $ make run
    $ make doc
