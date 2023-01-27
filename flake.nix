{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        ghcNames = [
          "ghc810"
          "ghc8107"
          "ghc90"
          "ghc902"
          "ghc92"
          "ghc924"
          "ghc925"
        ];
        inherit (nixpkgs.legacyPackages.${system}) callPackage;
        overlays = map (ghcName: (callPackage ./nix/haskellr-nixpkgs-overlay.nix {inherit ghcName;})) ghcNames;

        pkgs = import nixpkgs {
          inherit system overlays;
          config.allowUnfree = true;
        };
      in {
        packages = {
          inherit (pkgs.haskell.packages.ghc92) inline-r HaskellR-examples H IHaskell;
          HaskellR-site = pkgs.callPackage ./nix/haskellr-site.nix {};
        };
        overlays.default = pkgs.lib.composeManyExtensions overlays;
        formatter = pkgs.alejandra;
      }
    );
}
