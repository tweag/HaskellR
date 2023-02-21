{pkgs ? import ./nixpkgs.nix { }, ghc ? pkgs.haskell.compiler.ghc8107}:

with pkgs;

let
  # Uncomment the line below to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.

  # R = pkgs.R.override { enableStrictBarrier = true; };

  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack = if stdenv.isDarwin then {
      DYLD_LIBRARY_PATH = ["${R}/lib/R/lib"];
    } else {
      LD_LIBRARY_PATH = ["${R}/lib/R"];
    };
in

haskell.lib.buildStackProject ({
  name = "HaskellR";
  inherit ghc;
  buildInputs =
    [ python3Packages.ipython
      python3Packages.jupyter_client
      python3Packages.notebook
      R
      zeromq
      zlib
    ];
  LANG = "en_US.UTF-8";
  LD_LIBRARY_PATH = ["${R}/lib/R/"];
} // libHack)
