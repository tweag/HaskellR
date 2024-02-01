{
  pkgs ? import ./nixpkgs.nix { },
  ghc ? pkgs.haskell.compiler.ghc92,
  RVersion ? "4.2.3",
}:
let
  inherit (pkgs)
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

      patches =
        prevAttrs.patches or [ ]
        # R: 4.0.4 -> 4.1.0
        # See https://github.com/NixOS/nixpkgs/commit/9a88197fe7825f486052e3a9eca4a68192335978
        ++
          lists.optionals
            (
              RVersion == "4.1.0"
              && strings.hasInfix "--without-recommended-packages" (prevAttrs.preConfigure or "")
            )
            [
              (fetchpatch {
                name = "fix-tests-without-recommended-packages.patch";
                url = "https://github.com/wch/r-source/commit/7715c67cabe13bb15350cba1a78591bbb76c7bac.patch";
                # this part of the patch reverts something that was committed after R 4.1.0, so ignore it.
                excludes = [ "tests/Pkgs/xDir/pkg/DESCRIPTION" ];
                hash = "sha256-iguLndCIuKuCxVCi/4NSu+9RzBx5JyeHx3K6IhpYshQ=";
              })
              (fetchpatch {
                name = "use-codetools-conditionally.patch";
                url = "https://github.com/wch/r-source/commit/7543c28b931db386bb254e58995973493f88e30d.patch";
                hash = "sha256-+yHXB5AItFyQjSxfogxk72DrSDGiBh7OiLYFxou6Xlk=";
              })
            ]
        # R: 4.1.3 -> 4.2.0
        # See: https://github.com/NixOS/nixpkgs/commit/5eb9f35c44d30153ff1df2105ed73e148a79a3ee
        ++ lists.optionals (RVersion == "4.2.0" || RVersion == "4.2.1") [
          (fetchpatch {
            name = "test-reg-packages.patch";
            url = "https://raw.githubusercontent.com/NixOS/nixpkgs/5eb9f35c44d30153ff1df2105ed73e148a79a3ee/pkgs/applications/science/math/R/test-reg-packages.patch";
            hash = "sha256-FUzrenAFvD8GL1/RMG8DRRx+ITcEkDkRGKTVyAhyKqA=";
          })
        ];

      postPatch =
        prevAttrs.postPatch or ""
        # Nixpkgs ships with curl >= 8.x, which is not compatible with R pre-4.3. However, in the release notes for R
        # 4.3 (https://stat.ethz.ch/pipermail/r-announce/2023/000691.html), it is mentioned that despite the major
        # version change for curl, the API is still compatible with the previous version. Therefore, we can patch the
        # check for curl 7.x. This fixes the following error:
        #   error: libcurl >= 7.28.0 library and headers are required with support for https
        # We must patch the ./configure script and the ./m4/R.m4 file to make this work.
        + strings.optionalString (strings.versionOlder RVersion "4.3.0") ''
          substituteInPlace \
            ./configure \
            ./m4/R.m4 \
            --replace-fail \
              "#if LIBCURL_VERSION_MAJOR > 7" \
              "#if LIBCURL_VERSION_MAJOR < 7" \
            --replace-fail \
              "#elif LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 28" \
              "#elif LIBCURL_VERSION_MAJOR == 7 && LIBCURL_VERSION_MINOR >= 28 || LIBCURL_VERSION_MAJOR == 8"
        '';
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
