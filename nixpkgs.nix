let
  # NixOS/Nixpkgs master on 2025-06-07
  rev = "3e3afe5174c561dee0df6f2c2b2236990146329f";
  sha256 = "0dcslr2lwfaclfl4pmbwb3yw27bnvwlqiif394d3d66vyd163dvy";
in
import (
  fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  }
)
