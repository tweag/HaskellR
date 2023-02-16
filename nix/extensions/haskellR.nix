# Adds HaskellR to the package set for supported versions of GHC.
final: prev: let
  supportedGHCNames = [
    "ghc810"
    "ghc8107"

    "ghc90"
    "ghc902"

    "ghc92"
    "ghc924"
    "ghc925"
  ];

  supportedRNames = [
    "R_4_0_5"
    "R_4_1_3"
    "R_4_2_2"
    "R" # This is just an alias for R_4_2_2
  ];

  inherit (prev.lib) composeManyExtensions recursiveUpdate;
  inherit (prev) makeWrapper;
  inherit (prev.lib) makeBinPath;
  inherit (prev.lib.attrsets) cartesianProductOfSets;
  inherit (prev.lib.versions) major;

  prods = cartesianProductOfSets {
    ghcName = supportedGHCNames;
    rName = supportedRNames;
  };

  hpkgsDependencyFixes = haskell-final: haskell-prev: {
    # If we're using GHC pre 9.0, we need to fix some dependencies.
    # Nixpkgs has moved on to singletons >3.0, but we can't use that yet.
    singletons =
      if major haskell-prev.ghc.version == "8"
      then haskell-prev.callHackage "singletons" "2.7" {}
      else haskell-prev.singletons;
    # Nixpkgs provides th-desugar 1.12, but singletons 2.7 requires 1.11.
    th-desugar =
      if major haskell-prev.ghc.version == "8"
      then haskell-prev.callHackage "th-desugar" "1.11" {}
      else haskell-prev.th-desugar;
  };

  hpkgsOverrides = haskell-final: haskell-prev: let
    helper = rName: let
      R = prev.${rName};
      packageRenamer = packageName:
        if rName == "R"
        then packageName
        else "${packageName}-${rName}";
    in [
      {
        name = packageRenamer "inline-r";
        value = (haskell-final.callCabal2nix "inline-r" ../../inline-r {}).overrideAttrs (old: {
          propagatedBuildInputs = old.propagatedBuildInputs ++ [R];
        });
      }
      {
        name = packageRenamer "HaskellR-examples";
        value = (haskell-final.callCabal2nix "HaskellR-examples" ../../examples {}).overrideAttrs (old: {
          propagatedBuildInputs = old.propagatedBuildInputs ++ [R];
          nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
          postFixup = ''
            for f in $out/bin/*; do
              wrapProgram $f --prefix PATH : ${makeBinPath [R]}
            done
          '';
        });
      }
      {
        name = packageRenamer "H";
        value = (haskell-final.callCabal2nix "H" ../../H {}).overrideAttrs (old: {
          propagatedBuildInputs = old.propagatedBuildInputs ++ [haskell-prev.ghc];
          nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
          postFixup = ''
            wrapProgram $out/bin/H --prefix PATH : ${makeBinPath [haskell-prev.ghc]}
          '';
        });
      }
      {
        name = packageRenamer "IHaskell";
        value = haskell-final.callCabal2nix "IHaskell" ../../IHaskell {};
      }
    ];
    packages = builtins.listToAttrs (builtins.concatMap helper supportedRNames);
  in
    packages;

  # Given a Nix GHC name, return a function that can be used to override
  # that package set to add HaskellR and its dependencies.
  # The returned overlay contains the entirety of nixpkgs.
  HaskellROverlayer = ghcName: final: prev: let
    inherit (prev) makeWrapper;
    inherit (prev.lib) makeBinPath;
    overrides = composeManyExtensions [
      hpkgsDependencyFixes
      hpkgsOverrides
    ];

    new.haskell.packages.${ghcName} = prev.haskell.packages.${ghcName}.override {
      inherit overrides;
    };
  in
    recursiveUpdate prev new;

  overlays = builtins.map HaskellROverlayer supportedGHCNames;
  overlay = composeManyExtensions overlays;
in
  overlay final prev
