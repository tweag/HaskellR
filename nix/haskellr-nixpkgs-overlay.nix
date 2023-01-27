# Given a GHC version, return a function that can be used to override
# that package set to add HaskellR and its dependencies.
# The returned overlay contains the entirety of nixpkgs.
{ghcName}: final: prev: let
  inherit (prev) haskell R lib makeWrapper;
  inherit (lib) makeBinPath;
  hlib = haskell.lib;
  ghc = haskell.compiler.${ghcName};
in {
  haskell =
    prev.haskell
    // {
      packages =
        prev.haskell.packages
        // {
          ${ghcName} = prev.haskell.packages.${ghcName}.override {
            overrides = final: prev: let
              inherit (prev) callCabal2nix callHackage;
              depFixes =
                if ghcName < "ghc90"
                then {
                  # Nixpkgs has moved on to singletons >3.0, but we can't use that yet.
                  singletons = callHackage "singletons" "2.7" {};
                  # Nixpkgs provides th-desugar 1.12, but this singletons requires 1.11.
                  th-desugar = callHackage "th-desugar" "1.11" {};
                }
                else if ghcName < "ghc94"
                then {
                  # No changes necessary.
                }
                else throw "Unsupported GHC version ${ghcName}: HaskellR depend on ghc-parser which does not support GHC 9.3+";
            in
              depFixes
              // {
                # Add R to the propagated build inputs of inline-r.
                inline-r = (callCabal2nix "inline-r" ../inline-r {}).overrideAttrs (drv: {
                  propagatedBuildInputs = drv.propagatedBuildInputs ++ [R];
                });

                HaskellR-examples = (callCabal2nix "HaskellR-examples" ../examples {}).overrideAttrs (old: {
                  nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
                  # Add R to the PATH of the executables.
                  installPhase =
                    old.installPhase
                    + ''
                      for f in $out/bin/*; do
                        wrapProgram $f --prefix PATH : ${makeBinPath [R]}
                      done
                    '';
                });

                H = (callCabal2nix "H" ../H {}).overrideAttrs (old: {
                  propagatedBuildInputs = old.propagatedBuildInputs ++ [ghc];
                  nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
                  # The executable requires GHCI in the PATH.
                  installPhase =
                    old.installPhase
                    + ''
                      wrapProgram $out/bin/H --prefix PATH : ${makeBinPath [ghc]}
                    '';
                });

                IHaskell = callCabal2nix "IHaskell" ../IHaskell {};
              };
          };
        };
    };
}
