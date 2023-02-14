{
  pkgs ? import ./nixpkgs.nix {},
  mkShell ? pkgs.mkShell,
  callPackage ? pkgs.callPackage,
  pkg-config ? pkgs.pkg-config,
  ghc ? pkgs.haskellPackages.ghc,
  cabal-install ? pkgs.haskellPackages.cabal-install,
  stack ? pkgs.haskellPackages.stack,
}: let
  HaskellR = callPackage ./default.nix {};
in
  # Can be used to build HaskellR packages with `nix-shell` followed by:
  # stack --nix-pure build --stack-yaml <stack file>
  mkShell {
    buildInputs =
      HaskellR.buildInputs
      ++ [
        HaskellR.ghc
        cabal-install
        stack
        pkg-config
      ];
  }
