let clone = (import <nixpkgs> {}).fetchgit {
      url = git://github.com/datakurre/nixpkgs.git;
      rev = "da27ecee90e890bb416795863ebb59c5115e3f79";
      # Cloned from https://github.com/datakurre/nixpkgs.git, branch datakurre-R
      # (PR still not accepted in nixpkgs master)
      sha256 = "1j5357kvq6xqj7g11rqq4yiqgvvr9mak5m2wivjppdyacag7fnkz";
    };
in
with (import clone {}); stdenv.mkDerivation {
  name = "myEnv";
  buildInputs = [ python34Packages.ipython R zeromq pkgconfig zlib ghc ];
  STACK_IN_NIX_EXTRA_ARGS=''--extra-lib-dirs=${R}/lib/R/lib --extra-include-dirs=${R}/lib/R/include --extra-lib-dirs=${ghc}/lib --extra-include-dirs=${ghc}/include'';
  shellHook=''
    export DYLD_LIBRARY_PATH="${R}/lib/R/lib"
  '';
}

# Remember: for now on OSX this cannot be compiled in a pure Nix-shell
