let
  rev = "16448f9e76cf207a331e397b33b3e059b81fce0d";
  sha256 = "086kfpngcb0z30cb7w4chcv45djyknfg38kg91wb6xfxi3xs1f8h";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
