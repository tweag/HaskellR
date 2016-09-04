{pkgs ? import <nixpkgs> { }, ghc ? pkgs.haskell.compiler.ghc}:

# A shell file to build HaskellR against a version of R with
# the --enable-strict-barrier configure flag enabled for better memory
# diagnostics.

with pkgs;

let
  R = pkgs.R.override { enableStrictBarrier = true; };
in

haskell.lib.buildStackProject {
  name = "HaskellR";
  inherit ghc;
  buildInputs = [ ncurses python34Packages.ipython R zeromq zlib ];
  LANG = "en_US.UTF-8";
}
