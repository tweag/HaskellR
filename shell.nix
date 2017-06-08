{ bootstrap_pkgs ? import <nixpkgs> { }
, pkgs ? import (bootstrap_pkgs.fetchFromGitHub {
                  owner = "NixOS";
                  repo = "nixpkgs";
                  rev = "5b5f3f542a8d62f16ba360dfde42a98c7ee02e1a";
                  sha256 = "17f1yljk3vs67j8gvg12g2d2wfzgab4pbn3m3hs05wm368p45lyc";
                }) {}
, ghc ? pkgs.haskellPackages.ghc }:

with pkgs;

let
  # Uncomment the line below to build HaskellR against a version of R with
  # the --enable-strict-barrier configure flag enabled for better memory
  # diagnostics.

  # R = pkgs.R.override { enableStrictBarrier = true; };

  buildRPackage = pkgs.callPackage <nixpkgs/pkgs/development/r-modules/generic-builder.nix> {
    inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa Foundation;
    inherit (pkgs) R gettext gfortran;
  };


  rPackages = let self = rPackages; in pkgs.rPackages.override {
    overrides = {
      # keras needs reticulate >= 0.8
      reticulate = buildRPackage {
        name = "reticulate-0.8";
        src = pkgs.fetchzip {
          url = "https://mran.microsoft.com/src/contrib/reticulate_0.8.tar.gz";
          sha256 = "1y9zmcilgq00xvcl78r7h7qsc24mi11jf780xqzqila6p9v4y9k3";
        };
        nativeBuildInputs = [ self.Rcpp ];
        propagatedBuildInputs = [ self.Rcpp ];
        requireX = false;
      };
      # keras needs tensorflow >= 0.8.2; 0.8.2 is currently from
      # GitHub only.
      tensorflow = buildRPackage {
        name = "tensorflow-0.8.2";
        src = pkgs.fetchFromGitHub {
          owner = "rstudio";
          repo = "tensorflow";
          rev = "bace720e92a752f3b02de016c2abfdb93176a7a3";
          sha256 = "0lprr0q67z8m7wj3cxlwz95bw9rd8r5ma5qzjpb63vppnpjq9y8j";
        };
        nativeBuildInputs = with self; [ reticulate yaml jsonlite pkgs.python35Packages.tensorflow ];
        propagatedBuildInputs = with self; [ reticulate yaml jsonlite pkgs.python35Packages.tensorflow ];
        requireX = false;
      };

      # Using version from current GitHub master.
      keras = buildRPackage {
        name = "keras";
        src = pkgs.fetchFromGitHub {
          owner = "rstudio";
          repo = "keras";
          rev =  "809feb0828d7c21d614e1dad72790660e064a78b";
          sha256 = "1ky5z2456sc43mjy8jygvz2gj4qbxihsr27qrn0m67axlg3wxrih";
        };
        nativeBuildInputs = with self; [ reticulate tensorflow magrittr R6 ];
        propagatedBuildInputs =  with self; [ reticulate tensorflow magrittr R6 ];
        requireX = false;
      };
    };
  };
in

haskell.lib.buildStackProject {
  name = "HaskellR";
  inherit ghc;
  buildInputs =
    [ ncurses
      python35Packages.ipython
      python35Packages.jupyter_client
      python35Packages.notebook
      R
      zeromq
      zlib
      rPackages.keras
      which
    ];
  LANG = "en_US.UTF-8";
  LD_LIBRARY_PATH = ["${R}/lib/R/"];
}
