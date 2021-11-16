let
  rev = "27d463c120e453b5e741da47940d5ec25d1653db";
  sha256 = "10248qg08qxqx5y9j0kams3hs8zz0rdadw4n0gxch0hwgx2f6bmy";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
