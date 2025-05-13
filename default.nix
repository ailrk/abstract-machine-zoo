{ mkDerivation, base, containers, deepseq, lib, megaparsec, mtl
, parser-combinators, pretty-simple, sequence, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "abstract-machine-zoo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq megaparsec mtl parser-combinators
    pretty-simple sequence text unordered-containers vector
  ];
  executableHaskellDepends = [
    base containers deepseq megaparsec mtl parser-combinators
    pretty-simple sequence text unordered-containers vector
  ];
  doHaddock = false;
  license = lib.licenses.mit;
}
