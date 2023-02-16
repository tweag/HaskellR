{
  pkgs ? import ../nix/nixpkgs.nix {},
  mkShell ? pkgs.mkShell,
  callPackage ? pkgs.callPackage,
}:
mkShell {
  inherit (callPackage ./default.nix {}) buildInputs;
}
