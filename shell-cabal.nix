{
  pkgs ? import ./nixpkgs.nix { },
  ghcAttr ? "ghc98",
  RVersion ? "4.2.3",
}:
let
  inherit (pkgs)
    cabal-install
    cacert
    haskell
    pkg-config
    python3
    rPackages
    rWrapper
    stdenv
    zeromq
    zlib
    ;

  inherit (pkgs.lib) strings;

  # Set enableStrictBarrier to true to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.
  R = builtins.import ./nix/R.nix { inherit pkgs RVersion; };

  rEnv = rWrapper.override {
    inherit R;
    # ggplot2 is required for ./ihaskell-inline-r/examples/tutorial-ihaskell-inline-r.ipynb
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
haskell.packages.${ghcAttr}.shellFor rec {
  packages = haskellPackages: [ ];

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [
    cabal-install
    cacert
    python3Env
    rEnv
    zeromq
    zlib
  ];

  # Fixes https://github.com/commercialhaskell/stack/issues/2358
  LANG = "en_US.UTF-8";

  # XXX: workaround for https://ghc.haskell.org/trac/ghc/ticket/11042.
  ${(strings.optionalString stdenv.isDarwin "DY") + "LD_LIBRARY_PATH"} = strings.makeLibraryPath (
    [ ("${R}/lib/R" + (strings.optionalString stdenv.isDarwin "/lib")) ] ++ nativeBuildInputs
  );

  # Non-NixOS git needs cert
  GIT_SSL_CAINFO = "${cacert}/etc/ssl/certs/ca-bundle.crt";
}
