let
  rev = "e544ee88fa4590df75e221e645a03fe157a99e5b";
  sha256 = "0j8pdr9ymk7a2p8pamcbq2rbhlcg923i6abdmdm6496973s5gb34";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
