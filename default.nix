{ mkDerivation, base, lib }:
mkDerivation {
  pname = "abstract-machine-zoo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "abstract-machine-zoo";
}
