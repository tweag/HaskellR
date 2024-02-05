let
  # NixOS/Nixpkgs master on 2024-02-01
  rev = "a6fefb39e18b6ff828c04d59ea26d4988135bb88";
  sha256 = "sha256-lsnvxt/1KCUgrV8KURXdZXRo+mLUZdc7o5H0MvYFOHQ=";
in
import (
  fetchTarball {
    inherit sha256;
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  }
)
