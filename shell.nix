{
  pkgs ? import ./nixpkgs.nix {},
  stdenv ? pkgs.stdenv,
  buildStackProject ? pkgs.haskell.lib.buildStackProject,
  ghc ? pkgs.haskell.compiler.ghc8107,
  R ? pkgs.R,
  zeromq ? pkgs.zeromq,
  zlib ? pkgs.zlib,
  ipython ? pkgs.python3Packages.ipython,
  jupyter_client ? pkgs.python3Packages.jupyter_client,
  notebook ? pkgs.python3Packages.notebook,
}: let
  # Uncomment the line below to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.
  # R = pkgs.R.override { enableStrictBarrier = true; };
  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack =
    if stdenv.isDarwin
    then {
      DYLD_LIBRARY_PATH = ["${R}/lib/R/lib"];
    }
    else {
      LD_LIBRARY_PATH = ["${R}/lib/R"];
    };
in
  buildStackProject ({
      name = "HaskellR";
      inherit ghc;
      buildInputs = [
        R

        # Libraries
        zeromq
        zlib

        # Python packages
        ipython
        jupyter_client
        notebook
      ];
      LANG = "en_US.UTF-8";
    }
    // libHack)
