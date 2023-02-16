let
  # NixOS/Nixpkgs master 2023-02-14
  rev = "3bee4ab8db244db328b41ae5f4fbce19b658f1be";
  sha256 = "1ibdbknqc0mvn1jww4vhqb709sl1k3yzazx907snczm0f17grcr0";
in
  attrs:
    import (fetchTarball {
      inherit sha256;
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    }) ({
        overlays = [
          (import ./extensions/r.nix)
          (import ./extensions/haskellR.nix)
        ];
        # Required for HaskellR-examples
        config.allowUnfree = true;
      }
      // attrs)
