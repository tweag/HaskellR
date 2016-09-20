{pkgs ? import <nixpkgs> { }, ghc ? pkgs.haskell.compiler.ghc}:

with pkgs;

let
  # Uncomment the line below to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.

  # R = pkgs.R.override { enableStrictBarrier = true; };
in

haskell.lib.buildStackProject {
  name = "HaskellR";
  inherit ghc;
  buildInputs =
    [ ncurses
      python34Packages.ipython
      python34Packages.jupyter_client
      python34Packages.notebook
      R
      zeromq
      zlib
    ];
  LANG = "en_US.UTF-8";
  LD_LIBRARY_PATH = ["${R}/lib/R/"];
}
