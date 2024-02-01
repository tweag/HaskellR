{
  pkgs ? import ./nixpkgs.nix { },
  ghc ? pkgs.haskell.compiler.ghc92,
  RVersion ? "4.2.3",
}:
let
  inherit (pkgs)
    fetchurl
    haskell
    python3
    rPackages
    rWrapper
    stdenv
    zeromq
    zlib
    ;

  inherit (pkgs.lib) versions;

  # Set enableStrictBarrier to true to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.
  R = (pkgs.R.override { enableStrictBarrier = false; }).overrideAttrs (
    # Pin R to a specific version to avoid breaking changes in the R API.
    finalAttrs: prevAttrs: {
      version = RVersion;
      src = fetchurl {
        url = "https://cran.r-project.org/src/base/R-${versions.major finalAttrs.version}/R-${finalAttrs.version}.tar.gz";
        hash = builtins.getAttr RVersion {
          "4.0.0" = "sha256-Br6wKRtWmXhITrDctdIzlmXsdFc3vftOhz56WnVJKUA=";
          "4.0.1" = "sha256-lf4kpNjY+PiIRgyPX+QxHOxlbnoXItIzIYvAOGG8bzI=";
          "4.0.2" = "sha256-07zqs2TaCHZiXkCXgItCUSOV/fQSkvSRWrH9JXwbvnU=";
          "4.0.3" = "sha256-CZg6injV+2vEXSexxV+bpSZfePpUpVwTrmkfh8W7ng0=";
          "4.0.4" = "sha256-Uj8n1pdEoIyPC9Xh5sPYmk2yntmDOIunCWOjzTpKgC4=";
          "4.0.5" = "sha256-Cj7geap3LhMf5UNTEatif8vMtaUMq8VCkub2IEbx/+8=";
          "4.1.0" = "sha256-6OaJWdcoLKFHNg/JZEram9FhureBurFNM7iZmpUYJ4E=";
          "4.1.1" = "sha256-UV4DJldSJX0LcDbzgPguQrRu2Ec/VPJce2ftJbu902Q=";
          "4.1.2" = "sha256-IDYiXp9yB9TOCX5Ulyrs2qi0DX2ZEc0mSR+sWg+rOK8=";
          "4.1.3" = "sha256-Ff9bMzxhCUBgsqUunB2OxVzELdAp45yiKr2qkJUm/tY=";
          "4.2.0" = "sha256-OOq3cZt60JU4jwaqCQxaKyAnkZRd5g0+K7DqsfUJdIg=";
          "4.2.1" = "sha256-TVLbSG0nhI5UYT1O6XetlS7AjOF4B+G1JbEM1ENsZD8=";
          "4.2.2" = "sha256-D/YrQuxRr6VxPK7nxP3noMRZQLo5vvjFyUh/7wyVPfU=";
          "4.2.3" = "sha256-VeSpptQ74xTiwD0CZqb6VESv3OULMDv8O4Kzl5UW4HQ=";
          "4.3.0" = "sha256-RdzEi2zyfTYQIPd/3ho5IJ6Ze4FAKzZjyhwBAFampgk=";
          "4.3.1" = "sha256-jdC/JPECPG9hjDsxc4PSkbSklPQNc7mDrCL/6pnkupk=";
          "4.3.2" = "sha256-s/V2CsLu6AJqPw7vyyW0dyPZeAOO7o6ER2IJTIYMRSo=";
        };
      };
    }
  );

  rEnv = rWrapper.override {
    inherit R;
    # ggplot2 is required for ./IHaskell/examples/tutorial-ihaskell-inline-r.ipynb
    packages = with rPackages; [ ggplot2 ];
  };

  # NOTE: Workaround https://ghc.haskell.org/trac/ghc/ticket/11042.
  libHack =
    if stdenv.isDarwin then
      { DYLD_LIBRARY_PATH = [ "${R}/lib/R/lib" ]; }
    else
      { LD_LIBRARY_PATH = [ "${R}/lib/R" ]; };

  python3Env = python3.withPackages (
    ps: with ps; [
      ipython
      jupyter_client
      notebook
    ]
  );
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
