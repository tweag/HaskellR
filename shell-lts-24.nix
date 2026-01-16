{
  pkgs ? import ./nixpkgs.nix { },
  ghcAttr ? "ghc9102",
}:
let
  inherit (pkgs)
    cabal-install
    fetchpatch
    fetchurl
    haskell
    python3
    rPackages
    rWrapper
    stdenv
    zeromq
    zlib
    ;

  inherit (pkgs.lib) lists strings versions;

  # Set enableStrictBarrier to true to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.
  R = pkgs.R.override { enableStrictBarrier = false; };

  rEnv = rWrapper.override {
    inherit R;
    # ggplot2 is required for ./IHaskell/examples/tutorial-ihaskell-inline-r.ipynb
    packages = with rPackages; [ ggplot2 ];
  };

  python3Env = python3.withPackages (
    ps: with ps; [
      ipython
      jupyter_client
      notebook
    ]
  );
in
haskell.lib.buildStackProject {
  name = "HaskellR";
  ghc = pkgs.haskell.compiler.${ghcAttr};
  buildInputs = [
    cabal-install
    python3Env
    rEnv
    zeromq
    zlib
    pkgs.libsodium
  ];
  nativeBuildInputs = with pkgs; [
        pkg-config
    ];
  LANG = "en_US.UTF-8";
  # NOTE: Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  ${(strings.optionalString stdenv.isDarwin "DY") + "LD_LIBRARY_PATH"} = [
    ("${R}/lib/R" + (strings.optionalString stdenv.isDarwin "/lib"))
  ];
}
