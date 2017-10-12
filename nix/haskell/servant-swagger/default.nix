{ mkDerivation, aeson, aeson-qq, base, bytestring, Cabal
, cabal-doctest, directory, doctest, filepath, hspec, http-media
, insert-ordered-containers, lens, QuickCheck, servant, stdenv
, swagger2, text, time, unordered-containers
}:
mkDerivation {
  pname = "servant-swagger";
  version = "1.1.3.1";
  sha256 = "0n5vvrxg1lllkm385g0jd2j5bsr21bcibwn5szdpn6r5yh2mvn78";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson base bytestring hspec http-media insert-ordered-containers
    lens QuickCheck servant swagger2 text unordered-containers
  ];
  testHaskellDepends = [
    aeson aeson-qq base directory doctest filepath hspec lens
    QuickCheck servant swagger2 text time
  ];
  homepage = "https://github.com/haskell-servant/servant-swagger";
  description = "Generate Swagger specification for your servant API";
  license = stdenv.lib.licenses.bsd3;

  # Tests fail to compile
  doCheck = false;
}
