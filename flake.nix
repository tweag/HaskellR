{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "A flake for HaskellR, its website, and its examples";

  outputs = {
    self,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import ./nixpkgs.nix {
          inherit system;
        };

        inherit (pkgs) alejandra callPackage writeShellScriptBin;
        inherit (pkgs.lib) getExe;
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
      in {
        packages = {
          inherit HaskellR-site;
        };
        apps = {
          inherit HaskellR-site-serve;
        };
        formatter = alejandra;
      }
    );
}
