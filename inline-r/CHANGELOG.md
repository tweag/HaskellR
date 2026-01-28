# Change Log

## 1.1.x — (unreleased, placeholder)

* Setting limit in the interactive mode was disabled on darwin.
  In order to completely disable it use `H_DISABLE_INCREASE_STACK_SIZE` should
  be set.

## 1.0.2 - 2025-07-11

* Support R 4.5

## 1.0.1 - 2023-05-22
* Populate `R_LIBS` environment variable when starting R (#393)
* Map backslashes to forwardslashes in temp file names under Windows (#399)
* Relax upper bounds for GHC 9.6
* Fix imports for compatibility with mtl-2.3 (#414)

## 1.0.0 - 2022-11-11
* Support for R >= 4.2.
* Support for GHC 9 and GHC 9.2.
* Breaking change: remove `unhexp`, `pokeInfo`, `mark` and `named`.
* Breaking change: `HExp` no longer has a `Storable` instance.
* Breaking change: some fields of SEXPInfo have been removed.
* Breaking change: `Special` and `Primitive` constructors of `HExp` no
  longer carry any information. R-4.2 makes these forms completely
  opaque.
* Process quasiquotes using an instance of the R interpreter in
  a separate process. This improves support on macOS.

## 0.10.5 - 2020-11-16
* Support aeson >= 2

## 0.10.1 - 2018-03-12
* MonadFail instance to R.

## 0.10 - 2018-03-10
* inline-r supports running on FreeBSD
* Fixed Lock system during QQ-generation
* Support for new vector API.

## 0.9.2 - 2018-06-29
* GHC 8.4 compatibility
* Add Literal instance for 'Text'

## 0.9.1 - 2018-01-26
* Droped c2hs usage. Fixes installation on macOS.
* Fix QQ generation. Fixed possible resource free during QQ generation.

## 0.9.0.2 - 2016-10-23
* Fix build on ghc-8.2.
* Introduce Matcher API.

## 0.9.0 - 2016-06-20

* Breaking change: Rewrite of the `H.Prelude` module API.
* Reexport more modules from Language.R.
* Windows support for inline-r and H.
* Partially move away from c2hs internally: too many bugs on Windows.
* Export `PrintR` type class.
* Loosen the constraints of a few `HExp` constructors.
* Deprecate `parseFile`, `parseText`, `string` and `strings`.

## 0.8.0 - 2016-01-24

### Changed

* Rewritten R quasiquoter. Compile times now much faster than before
  for large quasiquotes.
* Assignments are now local by default. Use <<- to assign in global
  environment.

### Added

* vector-0.11 compatibility.
* Included in LTS-5.
* Vectors can now be sliced starting from arbitrary indexes. Slices
  were previously restricted to 0-based slices.

### Fixed

* Memory tests are now --enable-strict-barrier clean.
* Remove memory leak when allocating new vectors via
  `Data.Vector.SEXP` API.

## 0.7.3.0 - 2015-12-08

* Skip R's own signal handlers during init. They would otherwise
  interfere with signal delivery e.g. regarding socket conditions.
* stack --nix support.

## 0.7.2.0 - 2015-11-24

* OS X El Capitan support.

## 0.7.1.0 - 2015-09-14

* Fix vector copying.

## 0.7.0.0 - 2015-09-07

* First public release.
