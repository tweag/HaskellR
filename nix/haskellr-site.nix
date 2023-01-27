{
  stdenv,
  rubyPackages,
}: let
  inherit (rubyPackages) github-pages jekyll;
  name = "HaskellR-site";
  src = ../docs;
in
  stdenv.mkDerivation {
    inherit name src;
    buildInputs = [github-pages jekyll];
    # Use RUBYOPT=-Ku to force UTF-8 encoding.
    buildPhase = ''
      RUBYOPT=-Ku ${github-pages}/bin/github-pages build
    '';
    installPhase = ''
      # Copy the site to the output.
      mkdir -p $out
      cp -r _site $out/_site

      # Create a script to serve the site.
      mkdir -p $out/bin
      echo "#!${stdenv.shell}" > $out/bin/${name}
      echo '${jekyll}/bin/jekyll serve --baseurl "" --source ${src} "$@"' >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';
  }
