{
  pkgs ? import ./nixpkgs.nix {},
  ghc ? pkgs.haskell.compiler.ghc8107,
}:
let
  callPackage = pkgs.lib.flip pkgs.callPackage {inherit R;};
  # Set enableStrictBarrier to true to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.
  R = (pkgs.R.override {enableStrictBarrier = false;}).overrideAttrs (
    # Pin R to a specific version to avoid breaking changes in the R API.
    finalAttrs: prevAttrs: {
      version = "4.2.3";
      src = pkgs.fetchurl {
        url = "https://cran.r-project.org/src/base/R-${pkgs.lib.versions.major finalAttrs.version}/R-${finalAttrs.version}.tar.gz";
        sha256 = "sha256-VeSpptQ74xTiwD0CZqb6VESv3OULMDv8O4Kzl5UW4HQ=";
      };
    }
  );
in
callPackage (
  {
    fetchurl,
    haskell,
    lib,
    python3,
    rPackages,
    rWrapper,
    stdenv,
    zeromq,
    R,
    zlib,
  }:
  let
    # NOTE: Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
    libHack =
      if stdenv.isDarwin then
        {DYLD_LIBRARY_PATH = ["${R}/lib/R/lib"];}
      else
        {LD_LIBRARY_PATH = ["${R}/lib/R"];};

    python3Env = python3.withPackages (
      ps:
      with ps; [
        ipython
        jupyter_client
        notebook
      ]
    );

    rEnv = rWrapper.override {
      inherit R;
      # ggplot2 is required for ./IHaskell/examples/tutorial-ihaskell-inline-r.ipynb
      packages = with rPackages; [ggplot2];
    };
  in
  haskell.lib.buildStackProject (
    {
      name = "HaskellR";
      inherit ghc;
      buildInputs = [
        zeromq
        zlib
        python3Env
        rEnv
      ];
      LANG = "en_US.UTF-8";
    }
    // libHack
  )
)
