let
  # 2020-12-03
  rev = "39dbc77b99a1b98f98bc168b294033a4253b7337";
  sha256 = "0s8kxzk1m296sjm8msb9pxh5dy63v0sbd1mrkx25yajx8vl5zwfj";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
