# Adds the three latest versions of R to nixpkgs
final: prev: let
  rVersionsAndSha256s = {
    "4.0.5" = "sha256-Cj7geap3LhMf5UNTEatif8vMtaUMq8VCkub2IEbx/+8=";
    "4.1.3" = "sha256-Ff9bMzxhCUBgsqUunB2OxVzELdAp45yiKr2qkJUm/tY=";
    "4.2.2" = "sha256-D/YrQuxRr6VxPK7nxP3noMRZQLo5vvjFyUh/7wyVPfU=";
  };

  inherit (prev.lib.attrsets) mapAttrs';
  inherit (prev.lib.strings) replaceStrings;
  inherit (prev.lib.versions) major;
  derivationNamer = version: "R_${replaceStrings ["."] ["_"] version}";
  derivationCreator = version: sha256:
    prev.R.overrideAttrs (old: {
      inherit version;
      src = prev.fetchurl {
        url = "https://cran.r-project.org/src/base/R-${major version}/R-${version}.tar.gz";
        inherit sha256;
      };
    });
  deriver = version: sha256: {
    name = derivationNamer version;
    value = derivationCreator version sha256;
  };

  Rs = mapAttrs' deriver rVersionsAndSha256s;
in
  Rs
  // {
    # The default R version
    R = Rs.R_4_2_2;
  }
