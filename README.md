# The HaskellR project

[![Circle CI](https://circleci.com/gh/tweag/HaskellR.svg?style=svg&circle-token=203e28077ff516f587169b261f089d1e9d50693d)](https://circleci.com/gh/tweag/HaskellR)

* Website: https://tweag.github.io/HaskellR
* Mailing list: [Google Groups](https://groups.google.com/group/haskellr)

The HaskellR project provides an environment for efficiently
processing data using Haskell or R code, interchangeably. HaskellR
allows Haskell functions to seamlessly call R functions and *vice
versa*. It provides the Haskell programmer with the full breadth of
existing R libraries and extensions for numerical computation,
statistical analysis and machine learning.

## Getting Started & Documentation

All documentation is available on the
[HaskellR website](https://tweag.github.io/HaskellR).

## Developing HaskellR

If you wish to work on HaskellR itself or any of its constituent
packages, you'll need
[stack](https://github.com/commercialhaskell/stack). Once installed,
check out the official source repository using

```
# Equivalent to git clone https://github.com/tweag/HaskellR
$ hub clone tweag/HaskellR
```

Then, you can build all the packages in the project using

```
$ stack setup
$ stack build
```

See the
[stack documentation](https://github.com/commercialhaskell/stack/wiki)
for further information on how to run tests, benchmarks, or build the
API documentation. You can do all of that at once with

```
$ stack build --test --haddock --bench
```

Optionally, pass in the `--docker` flag to all commands if you wish to
develop inside a Docker container for more reproducible builds.

Optionally, pass in the `--nix` flag to all commands if you have the
[Nix][nix] package manager installed. Nix can populate a *local* build
environment including all necessary system dependencies without
touching your global filesystem. Use it as a cross-platform
alternative to Docker.

[nix]: http://nixos.org/nix

## License

Copyright (c) 2013-2015 Amgen, Inc.  
Copyright (c) 2015 Tweag I/O Limited.

All rights reserved.

HaskellR is free software, and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

## About

![Tweag I/O](http://i.imgur.com/0HK8X4y.png)

HaskellR is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).
