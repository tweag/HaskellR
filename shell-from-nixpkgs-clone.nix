let pkgs = (import ~/fix/nixpkgs {});
in
with pkgs; stdenv.mkDerivation {
  name = "myEnv";
  buildInputs = [ python34Packages.ipython R zeromq pkgconfig zlib ghc ];
  #STACK_IN_NIX_EXTRA_ARGS=''--extra-lib-dirs=${R}/lib/R/lib --extra-include-dirs=${R}/lib/R/include --extra-lib-dirs=${ghc}/lib --extra-include-dirs=${ghc}/include'';
  shellHook=''
    export DYLD_LIBRARY_PATH="${R}/lib/R/lib"
  '';
}

# Remember: for now on OSX this cannot be compiled in a pure Nix-shell
