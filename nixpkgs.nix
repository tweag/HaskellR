let
  # 2020-12-03
  rev = "cb52887c38ef47f6649a21336cfaa347cc7dfa75";
  sha256 = "1i84hzcdl3fa3jl80yp7dmqx3pxaaqdiif589q10whriagvb3cp1";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
