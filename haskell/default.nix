{ mkDerivation, base, containers, either, HUnit, lens-family-core
, mtl, mvc, pipes, QuickCheck, safe, stdenv, test-framework
, test-framework-hunit
}:
mkDerivation {
  pname = "afgame";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers either lens-family-core mtl mvc pipes QuickCheck
    safe
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base HUnit test-framework test-framework-hunit
  ];
  license = stdenv.lib.licenses.unfree;
}
