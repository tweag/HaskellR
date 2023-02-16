# Can be built with:
#   nix-build --no-sandbox -E 'with import ./nix/nixpkgs.nix {}; callPackage ./default.nix {}'
{
  stdenv,
  haskell,
  ghc,
  R,
  zeromq,
  zlib,
  python3Packages,
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

  inherit (haskell.lib) buildStackProject;
  inherit (python3Packages) ipython jupyter_client notebook;
in
  buildStackProject ({
      name = "HaskellR";
      inherit ghc;
      src = ./.;
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
