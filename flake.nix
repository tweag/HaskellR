{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "A flake for HaskellR, its website, and its examples";

  outputs = {
    self,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import ./nix/nixpkgs.nix {
          inherit system;
        };

        HaskellR-site = pkgs.callPackage ./docs {};
        HaskellR-site-serve = {
          type = "app";
          program = let
            name = "HaskellR-site-serve";
            script = ''
              #!${pkgs.stdenv.shell}
              ${pkgs.lib.getExe pkgs.rubyPackages.jekyll} serve \
                --baseurl "/HaskellR" \
                --source ${HaskellR-site}/share/_site \
                "$@"
            '';
          in
            pkgs.lib.getExe (pkgs.writeShellScriptBin name script);
        };
      in {
        overlay = pkgs.lib.composeManyExtensions pkgs.overlays;
        packages = {
          inherit HaskellR-site;
          inherit (pkgs.haskellPackages) inline-r H IHaskell HaskellR-examples;
        };
        apps = {
          inherit HaskellR-site-serve;
        };
        formatter = pkgs.alejandra;
      }
    );
}
