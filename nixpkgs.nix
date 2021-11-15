let
  rev = "c114cd459e1bac0d05e4e27c9ea43c99799cbbd1";
  sha256 = "1hpspw8jbzq03k8a6izxnbqww42mihqaa6c8nszw30igiaqc98i2";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
