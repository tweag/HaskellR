# Can be built with:
#   nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix {}'
{
  stdenv,
  rubyPackages,
}: let
  inherit (rubyPackages) github-pages jekyll;
in
  stdenv.mkDerivation {
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
