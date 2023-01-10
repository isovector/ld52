{ mkDerivation, aeson, base, fetchgit, lib, text }:
mkDerivation {
  pname = "ldtk-types";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/isovector/ldtk-types";
    sha256 = "1b1kkk1kjzb1vwhra900846kz2rb2krya7ij8klkcnglxh9wi413";
    rev = "0be47b237f36e4a16c6fac5d384adc733ed07711";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ aeson base text ];
  testHaskellDepends = [ aeson base text ];
  homepage = "https://github.com/isovector/ldtk-types#readme";
  description = "Datatypes and Aeson instances for parsing LDtk";
  license = lib.licenses.bsd3;
}
