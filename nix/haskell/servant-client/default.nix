{ mkDerivation, aeson, attoparsec, base, base-compat, bytestring
, containers, deepseq, exceptions, generics-sop, hspec
, http-api-data, http-client, http-client-tls, http-media
, http-types, HUnit, monad-control, mtl, network, QuickCheck
, semigroupoids, servant, servant-client-core, servant-server
, stdenv, text, transformers, transformers-base
, transformers-compat, wai, warp, fetchgit
}:
mkDerivation {
  pname = "servant-client";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "08b5z96v1b3izs2rd2fra35w03kb69wb9r68ia2r489kbynz19ch";
    rev = "ce355147d0bf3d1eefc1dc049f1cfa3f008d38c9";
  }) + "/servant-client";
  libraryHaskellDepends = [
    aeson attoparsec base base-compat bytestring containers exceptions
    http-client http-client-tls http-media http-types monad-control mtl
    semigroupoids servant-client-core text transformers
    transformers-base transformers-compat
  ];
  testHaskellDepends = [
    aeson base base-compat bytestring containers deepseq generics-sop
    hspec http-api-data http-client http-media http-types HUnit mtl
    network QuickCheck servant servant-client-core servant-server text
    transformers transformers-compat wai warp
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices";
  license = stdenv.lib.licenses.bsd3;
}
