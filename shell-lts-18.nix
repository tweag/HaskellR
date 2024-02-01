let
  pkgs = import ./nixpkgs.nix { };
  ghc = pkgs.haskell.compiler.ghc810;
in
builtins.import ./shell.nix { inherit pkgs ghc; }
