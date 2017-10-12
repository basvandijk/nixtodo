{ mkDerivation, base, base-compat, base64-bytestring, bytestring
, containers, deepseq, exceptions, generics-sop, hspec
, http-api-data, http-media, http-types, mtl, network-uri
, QuickCheck, safe, servant, stdenv, text, fetchgit
}:
mkDerivation {
  pname = "servant-client-core";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "08b5z96v1b3izs2rd2fra35w03kb69wb9r68ia2r489kbynz19ch";
    rev = "ce355147d0bf3d1eefc1dc049f1cfa3f008d38c9";
  }) + "/servant-client-core";
  libraryHaskellDepends = [
    base base-compat base64-bytestring bytestring containers exceptions
    generics-sop http-api-data http-media http-types mtl network-uri
    safe servant text
  ];
  testHaskellDepends = [ base base-compat deepseq hspec QuickCheck ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "Core functionality and class for client function generation for servant APIs";
  license = stdenv.lib.licenses.bsd3;
}
