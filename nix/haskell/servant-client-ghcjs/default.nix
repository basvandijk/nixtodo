{ mkDerivation, base, bytestring, case-insensitive, containers
, exceptions, ghcjs-base, ghcjs-prim, http-types, monad-control
, mtl, semigroupoids, servant-client-core, stdenv
, string-conversions, transformers, transformers-base, fetchgit
}:
mkDerivation {
  pname = "servant-client-ghcjs";
  version = "0.11";
  src = (fetchgit {
    url = "https://github.com/LumiGuide/servant.git";
    sha256 = "08b5z96v1b3izs2rd2fra35w03kb69wb9r68ia2r489kbynz19ch";
    rev = "ce355147d0bf3d1eefc1dc049f1cfa3f008d38c9";
  }) + "/servant-client-ghcjs";
  libraryHaskellDepends = [
    base bytestring case-insensitive containers exceptions ghcjs-base
    ghcjs-prim http-types monad-control mtl semigroupoids
    servant-client-core string-conversions transformers
    transformers-base
  ];
  homepage = "http://haskell-servant.readthedocs.org/";
  description = "automatical derivation of querying functions for servant webservices for ghcjs";
  license = stdenv.lib.licenses.bsd3;
}
