{ pkgs ? import ./nixpkgs.nix { }, ghc ? pkgs.haskell.compiler.ghc8107 }:

with pkgs;

let
  # Uncomment the line below to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.

  # R = pkgs.R.override { enableStrictBarrier = true; };

  # XXX Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack =
    if stdenv.isDarwin
    then { DYLD_LIBRARY_PATH = [ "${R}/lib/R/lib" ]; }
    else { LD_LIBRARY_PATH = [ "${R}/lib/R" ]; }
  ;

  python3Env = python3.withPackages (ps: with ps; [
    ipython
    jupyter_client
    notebook
  ]);

  rEnv = rWrapper.override {
    packages = with rPackages; [
      # ggplot2 is required for ./IHaskell/examples/tutorial-ihaskell-inline-r.ipynb
      ggplot2
      optimx
      Rcpp
      signal
      tuneR
    ];
  };
in

haskell.lib.buildStackProject ({
  name = "HaskellR";
  inherit ghc;
  buildInputs = [
    zeromq
    zlib
    python3Env
    rEnv
  ];
  LANG = "en_US.UTF-8";
} // libHack)
