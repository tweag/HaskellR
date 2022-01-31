let
  rev = "c61a33bc8bcc3cb7c5dcfb302b6642a57483ced9";
  sha256 = "1iad51y2fnpklql58515adrfybbsnlv80l3yv0d026qjif887llg";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
