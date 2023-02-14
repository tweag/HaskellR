{
  pkgs ? import ../nixpkgs.nix {},
  mkDerivation ? pkgs.stdenv.mkDerivation,
  rubyPackages ? pkgs.rubyPackages,
}: let
  inherit (rubyPackages) github-pages jekyll;
in
  mkDerivation {
    name = "HaskellR-site";
    src = ./.;
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
