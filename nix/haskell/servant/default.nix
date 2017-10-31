{ mkDerivation, aeson, aeson-compat, attoparsec, base, base-compat
, bytestring, Cabal, cabal-doctest, case-insensitive, directory
, doctest, filemanip, filepath, hspec, http-api-data, http-media
, http-types, mmorph, mtl, natural-transformation, network-uri
, QuickCheck, quickcheck-instances, stdenv, string-conversions
, tagged, text, url, vault, fetchFromGitHub
}:
mkDerivation {
  pname = "servant";
  version = "0.11";
  src = (fetchFromGitHub {
    owner = "LumiGuide";
    repo = "servant";
    sha256 = "08b5z96v1b3izs2rd2fra35w03kb69wb9r68ia2r489kbynz19ch";
    rev = "ce355147d0bf3d1eefc1dc049f1cfa3f008d38c9";
  }) + "/servant";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring case-insensitive
    http-api-data http-media http-types mmorph mtl
    natural-transformation network-uri string-conversions tagged text
    vault
  ];
  testHaskellDepends = [
    aeson aeson-compat attoparsec base base-compat bytestring directory
    doctest filemanip filepath hspec QuickCheck quickcheck-instances
    string-conversions text url
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "A family of combinators for defining webservices APIs";
  license = stdenv.lib.licenses.bsd3;
}
