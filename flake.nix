{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  description = "A flake for HaskellR, its website, and its examples";

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };

        HaskellR-site = pkgs.stdenv.mkDerivation (
          let
            name = "HaskellR-site";
            src = ./docs;
            inherit (pkgs.rubyPackages) github-pages jekyll;
          in {
            inherit name src;
            buildInputs = [github-pages jekyll];
            # Use RUBYOPT=-Ku to force UTF-8 encoding.
            buildPhase = ''
              RUBYOPT=-Ku ${github-pages}/bin/github-pages build
            '';
            installPhase = ''
              # Copy the site to the output.
              mkdir -p $out/share
              cp -r _site $out/share/_site
            '';
          }
        );
      in {
        packages = {
          inherit HaskellR-site;
        };
        apps.HaskellR-site-serve = {
          type = "app";
          program = let
            inherit (pkgs) writeShellScriptBin;
            inherit (pkgs.stdenv) shell;
            inherit (pkgs.rubyPackages) jekyll;
            name = "HaskellR-site-serve";
          in
            (writeShellScriptBin name ''
              #!${shell}
              ${jekyll}/bin/jekyll serve --baseurl "" --source ${HaskellR-site}/share/_site "$@"
            '')
            .outPath
            + "/bin/${name}";
        };
        formatter = pkgs.alejandra;
      }
    );
}
