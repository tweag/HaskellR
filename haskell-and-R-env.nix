/* This file shows how to create a nix derivation defining an environment containing GHC with HaskellR and R with desired libraries */

let
fH = { mkDerivation, base, bytestring, cmdargs, directory, file-embed
, inline-r, pretty, process, singletons, stdenv, tasty
, tasty-golden, tasty-hunit, temporary-rc, text, vector
, R
}:
mkDerivation {
  pname = "H";
  version = "0.7.0.0";
  src = ./H;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring cmdargs file-embed inline-r pretty process
    temporary-rc vector
  ];
  testHaskellDepends = [ R
    base bytestring directory inline-r process singletons tasty
    tasty-golden tasty-hunit text vector
  ];
  description = "The Haskell/R mixed programming environment";
  license = stdenv.lib.licenses.bsd3;
};

fInlineR = { mkDerivation, aeson, base, bytestring, c2hs, data-default-class
, deepseq, directory, exceptions, filepath, ieee754, mtl, pretty
, primitive, process, quickcheck-assertions, setenv, silently
, singletons, stdenv, strict, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, text, th-lift
, th-orphans, transformers, unix, vector
, pkgconfig, R
}:
mkDerivation {
  pname = "inline-r";
  version = "0.7.2.0";
  src = ./inline-r;
  libraryHaskellDepends = [
    aeson base bytestring data-default-class deepseq exceptions mtl
    pretty primitive process setenv singletons template-haskell text
    th-lift th-orphans transformers unix vector
  ];
  libraryPkgconfigDepends = [ R ];
  libraryToolDepends = [ c2hs pkgconfig ];
  testHaskellDepends = [
    base bytestring directory filepath ieee754 mtl process
    quickcheck-assertions silently singletons strict tasty tasty-golden
    tasty-hunit tasty-quickcheck template-haskell temporary text unix
    vector
  ];
  description = "Seamlessly call R from Haskell and vice versa. No FFI required.";
  license = stdenv.lib.licenses.bsd3;
};

fIHaskellInlineR = { mkDerivation, base, base64-bytestring, blaze-html, bytestring
, filepath, ihaskell, ihaskell-blaze, inline-r, stdenv
, template-haskell, temporary
}:
mkDerivation {
  pname = "ihaskell-inline-r";
  version = "0.1.0.0";
  src = ./IHaskell;
  libraryHaskellDepends = [
    base base64-bytestring blaze-html bytestring filepath ihaskell
    ihaskell-blaze inline-r template-haskell temporary
  ];
  homepage = "https://tweag.github.io/HaskellR/";
  description = "Embed R quasiquotes and plots in IHaskell notebooks";
  license = stdenv.lib.licenses.bsd3;
};

fExamples = { mkDerivation, base, deepseq, inline-r, integration, mwc-random
, stdenv, temporary-rc, R
}:
mkDerivation {
  pname = "HaskellR-examples";
  version = "0.1.0.0";
  src = ./examples;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ R
    base deepseq inline-r integration mwc-random temporary-rc
  ];
  description = "Examples bundled with the HaskellR project";
  license = stdenv.lib.licenses.unfree;
};

in
with (import ((import <nixpkgs> {}).fetchgit {
   url = git://github.com/NixOS/nixpkgs;
   rev = "e354ff9a24e90cb623ed0b51b53fe3795e6468ca";
   sha256 = "04iifkamqwc5fq9pw7dzz98bwbsk6f8fil99ls3mhkwvzwc6drg0";
}) {});
#with (import <nixpkgs> {});
let h = haskell.packages.lts-3_13; in
rec {
   H = (h.callPackage fH { inherit inline-r; }).overrideDerivation (attrs:
       { doCheck = false; });
   inline-r = h.callPackage fInlineR {};
   IHaskell-inline-r = h.callPackage fIHaskellInlineR { inherit inline-r; };
   examples = h.callPackage fExamples { inherit inline-r; };
   ghcEnv = stdenv.mkDerivation {
              name = "env";
              buildInputs = [(rWrapper.override {
                                packages = with rPackages; [ numDeriv optimx /* insert rPackages.XXXXXX here */ ];
                             })
                             examples
                             (h.ghcWithPackages (p: [ H inline-r ]))];
            };
}
