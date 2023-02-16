# Returns a list of derivations supported by HaskellR
# Can override the supported GHC and R versions from the command line with the --arg 
# supportedGHCNames and --arg supportedRNames flags.
# Example: To build all the derivations for GHC 8.10.7 and R 4.0.5, use
# nix-build ./ci.nix --arg supportedGHCNames '["ghc8107"]' --arg supportedRNames '["R_4_0_5"]'
{
  pkgs ? (import ./nixpkgs.nix {} ),
  supportedGHCNames ? [
    "ghc810"
    "ghc8107"

    "ghc90"
    "ghc902"

    "ghc92"
    "ghc924"
    "ghc925"
  ],
  # Because the latest version of R is just an alias to the latest
  # entry in this list, we don't need to explicitly include it.
  supportedRNames ? [
    "R_4_0_5"
    "R_4_1_3"
    "R_4_2_2"
  ],
}: let
  inherit (pkgs) lib;
  inherit (lib.attrsets) cartesianProductOfSets;

  combinations =
    cartesianProductOfSets
    {
      ghcName = supportedGHCNames;
      rName = supportedRNames;
      packageName = ["inline-r" "HaskellR-examples" "H" "IHaskell"];
    };

  mapper = {
    ghcName,
    rName,
    packageName,
  }: {
    name = "${ghcName}-${packageName}-${rName}";
    value = pkgs.haskell.packages.${ghcName}."${packageName}-${rName}";
  };
  derivations = builtins.listToAttrs (builtins.map mapper combinations);
in
lib.trace (builtins.toJSON derivations)  derivations
