{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-22.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

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
        lib = pkgs.lib;
        hlib = pkgs.haskell.lib;
        hpkgs =
          pkgs.haskell.packages.ghc8107.override
          {
            overrides = final: prev: {
              # Nixpkgs has moved on to singletons >3.0, but we can't use that yet.
              singletons = hpkgs.callHackage "singletons" "2.7" {};
              # Nixpkgs provides th-desugar 1.12, but this singletons requires 1.11.
              th-desugar = hpkgs.callHackage "th-desugar" "1.11" {};

              # Add pkgs.R to the propagated build inputs of inline-r.
              inline-r = (hpkgs.callCabal2nix "inline-r" ./inline-r {}).overrideAttrs (drv: {
                propagatedBuildInputs = drv.propagatedBuildInputs ++ [pkgs.R];
              });

              HaskellR-examples = (hpkgs.callCabal2nix "HaskellR-examples" ./examples {}).overrideAttrs (old: {
                nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.makeWrapper];
                # Add pkgs.R to the PATH of the executables.
                installPhase =
                  old.installPhase
                  + ''
                    for f in $out/bin/*; do
                      wrapProgram $f --prefix PATH : ${lib.makeBinPath [pkgs.R]}
                    done
                  '';
              });

              H = (hpkgs.callCabal2nix "H" ./H {}).overrideAttrs (old: let
                ghc = pkgs.haskell.compiler.ghc8107;
              in {
                propagatedBuildInputs = old.propagatedBuildInputs ++ [ghc];
                nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.makeWrapper];
                # The executable requires GHCI in the PATH.
                installPhase =
                  old.installPhase
                  + ''
                    wrapProgram $out/bin/H --prefix PATH : ${lib.makeBinPath [ghc]}
                  '';
              });

              IHaskell = hpkgs.callCabal2nix "IHaskell" ./IHaskell {};
            };
          };

        HaskellR-site = pkgs.stdenv.mkDerivation (
          let
            name = "HaskellR-site";
            src = ./docs;
            github-pages = pkgs.rubyPackages.github-pages;
            jekyll = pkgs.rubyPackages.jekyll;
          in {
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
              echo "#!${pkgs.stdenv.shell}" > $out/bin/${name}
              echo '${jekyll}/bin/jekyll serve --baseurl "" --source ${src} "$@"' >> $out/bin/${name}
              chmod +x $out/bin/${name}
            '';
          }
        );
        #     LD_LIBRARY_PATH = ["${R}/lib/R"];
      in {
        packages = {
          inherit (hpkgs) inline-r HaskellR-examples H IHaskell;
          inherit HaskellR-site;
        };
        # packages.default = hlib.buildStackProject ({
        #     name = "HaskellR";
        #     inherit ghc;
        #     buildInputs = with pkgs; [
        #       # python3Packages.ipython
        #       # python3Packages.jupyter_client
        #       # python3Packages.notebook
        #       R
        #       # zeromq
        #       # zlib
        #     ];
        #     LANG = "en_US.UTF-8";
        #   });
        #   # // libHack);
        formatter = pkgs.alejandra;
      }
    );
}
