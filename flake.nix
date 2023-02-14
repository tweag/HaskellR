{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "A flake for HaskellR, its website, and its examples";

  outputs = {
    self,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        # All the versions of GHC that we support.
        defaultGHCName = "ghc8107";
        supportedGHCNames = [
          "ghc810"
          "ghc8107"
          "ghc90"
          "ghc902"
          "ghc92"
          "ghc924"
          "ghc925"
        ];

        # Given a Nix GHC name, return a function that can be used to override
        # that package set to add HaskellR and its dependencies.
        # The returned overlay contains the entirety of nixpkgs.
        HaskellR-Overlayer = ghcName: final: prev: let
          inherit (prev) R lib makeWrapper;
          inherit (lib) makeBinPath recursiveUpdate versions;
          overrides = haskell-final: haskell-prev: {
            # If we're using GHC pre 9.0, we need to fix some dependencies.
            # Nixpkgs has moved on to singletons >3.0, but we can't use that yet.
            singletons =
              if versions.major haskell-prev.ghc.version == "8"
              then haskell-prev.callHackage "singletons" "2.7" {}
              else haskell-prev.singletons;
            # Nixpkgs provides th-desugar 1.12, but singletons 2.7 requires 1.11.
            th-desugar =
              if versions.major haskell-prev.ghc.version == "8"
              then haskell-prev.callHackage "th-desugar" "1.11" {}
              else haskell-prev.th-desugar;

            inline-r = (haskell-final.callCabal2nix "inline-r" ./inline-r {}).overrideAttrs (old: {
              # Add R to the propagated build inputs of inline-r.
              propagatedBuildInputs = old.propagatedBuildInputs ++ [R];
            });

            HaskellR-examples = (haskell-final.callCabal2nix "HaskellR-examples" ./examples {}).overrideAttrs (old: {
              # Add R to the propagated build inputs of inline-r.
              propagatedBuildInputs = old.propagatedBuildInputs ++ [R];
              nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
              # Add R to the PATH of the executables.
              postFixup = ''
                for f in $out/bin/*; do
                  wrapProgram $f --prefix PATH : ${makeBinPath [R]}
                done
              '';
            });

            H = (haskell-final.callCabal2nix "H" ./H {}).overrideAttrs (old: {
              propagatedBuildInputs = old.propagatedBuildInputs ++ [haskell-prev.ghc];
              nativeBuildInputs = old.nativeBuildInputs ++ [makeWrapper];
              # The executable requires GHCI in the PATH.
              postFixup = ''
                wrapProgram $out/bin/H --prefix PATH : ${makeBinPath [haskell-prev.ghc]}
              '';
            });

            IHaskell = haskell-final.callCabal2nix "IHaskell" ./IHaskell {};
          };
        in
          recursiveUpdate
          prev
          {
            haskell.packages.${ghcName} = prev.haskell.packages.${ghcName}.override {
              inherit overrides;
            };
          };

        overlays = builtins.map HaskellR-Overlayer supportedGHCNames;
        pkgs = import ./nixpkgs.nix {
          inherit system overlays;
          # Required for HaskellR-examples
          config.allowUnfree = true;
        };

        inherit (pkgs) alejandra callPackage writeShellScriptBin;
        inherit (pkgs.lib) attrsets getExe lists;
        inherit (pkgs.rubyPackages) jekyll;
        inherit (pkgs.stdenv) shell;

        HaskellR-site = callPackage ./docs {};
        HaskellR-site-serve = {
          type = "app";
          program = let
            name = "HaskellR-site-serve";
            script = ''
              #!${shell}
              ${getExe jekyll} serve \
                --baseurl "/HaskellR" \
                --source ${HaskellR-site}/share/_site \
                "$@"
            '';
          in
            getExe (writeShellScriptBin name script);
        };
        HaskellR-Packages = builtins.listToAttrs (lists.crossLists
          (ghcName: packageName: {
            name = "${ghcName}-${packageName}";
            value = pkgs.haskell.packages.${ghcName}.${packageName};
          }) [supportedGHCNames ["inline-r" "HaskellR-examples" "H" "IHaskell"]]);
      in {
        overlays.default = pkgs.lib.composeManyExtensions overlays;
        packages =
          {
            inherit HaskellR-site;
          }
          // HaskellR-Packages;
        apps = {
          inherit HaskellR-site-serve;
        };
        formatter = alejandra;
      }
    );
}
