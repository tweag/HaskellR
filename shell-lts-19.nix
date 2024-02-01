let
  pkgs = import ./nixpkgs.nix { };
  ghc = pkgs.haskell.compiler.ghc90;
in
builtins.import ./shell.nix { inherit pkgs ghc; }
