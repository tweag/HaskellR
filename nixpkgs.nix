let
  # NixOS/Nixpkgs master on 2024-02-01
#  rev = "a6fefb39e18b6ff828c04d59ea26d4988135bb88";
  rev = "3e3afe5174c5";
  sha256 = "0dcslr2lwfaclfl4pmbwb3yw27bnvwlqiif394d3d66vyd163dvy";
in
import (
  fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  }
)
