# Change Log

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
