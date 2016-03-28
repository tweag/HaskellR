# A shell file to build HaskellR against a version of R with
# the --enable-strict-barrier configure flag enabled for better memory
# diagnostics.

with (import <nixpkgs> { });

with stdenv.lib;

let
  R = pkgs.R.override { enableStrictBarrier = true; };
in

haskell.lib.buildStackProject {
  name = "HaskellR";
  buildInputs = [ ncurses pkgconfig python34Packages.ipython R zeromq zlib ];
  LANG = "en_US.UTF-8";
}
