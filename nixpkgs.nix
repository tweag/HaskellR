let
  # NixOS/Nixpkgs master 2023-11-29
  rev = "28952798b6803c0f5e4d2b154cbdcb37c80dd15d";
  sha256 = "sha256-nGqnojVDm2TI27Ypq+X5DpvFHILRhbmOJSjA+T8EwR4=";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
