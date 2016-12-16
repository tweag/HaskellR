with (import <nixpkgs> { });

stdenv.mkDerivation rec {
  name = "HaskellR-site";

  buildInputs = [ (bundlerEnv {
    name = "HaskellR-site-bundler";
    gemfile = ./Gemfile;
    lockfile = ./Gemfile.lock;
    gemset = ./gemset.nix;
  }) jekyll nodejs ];
}
