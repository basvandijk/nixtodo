# This Nix expression describes how we override and extend the Haskell packages
# from <nixpkgs>.

pkgs : {

  overrides = self: super:
    let
      thirdPartyHsPkgs = let dir = ./haskell; in
        pkgs.lib.genAttrs
          (builtins.attrNames (builtins.readDir dir))
          (name : self.callPackage (dir + "/${name}") {});

      hsPkgs = let dir = <nixtodo/hs-pkgs>; in
        pkgs.lib.genAttrs
          (builtins.filter
            (name: builtins.pathExists (dir + "/${name}/${name}.cabal"))
            (builtins.attrNames (builtins.readDir dir)))
          (name : let pkgDir = dir + "/${name}";
                      drv'' = self.callPackage (
                        super.haskellSrc2nix {
                          inherit name;
                          src = pkgDir + "/${name}.cabal";
                        }
                      ) {};
                      drv' = pkgs.haskell.lib.overrideCabal drv'' (_drv : {
                        src = pkgs.lib.sourceByRegex pkgDir [
                                "^${name}.cabal$"
                                "^LICENSE$"
                                ".*\.hs"
                                "^src$"     "^src/.*"
                                "^test$"    "^test/.*"
                                "^include$" "^include/.*"
                              ];
                      });
                      overridePath = pkgDir + "/override.nix";
                      drv = if builtins.pathExists overridePath
                            then import overridePath pkgs drv'
                            else drv';
                  in drv
          );

    in thirdPartyHsPkgs // hsPkgs // {

      miso = let version = "0.8.0.0"; in super.callPackage (super.haskellSrc2nix {
        name = "miso-${version}";
        src = pkgs.fetchurl {
          url = "http://hackage.haskell.org/package/miso-${version}/miso.cabal";
          sha256 = "1xbz76pvpapbl3xsb5g1355pslg1b56xnm1x0xvapyyf2y2x8w47";
        };
        sha256 = "1z49dd3g30fhk6kvm5lfrzapsbf3381bmgyzsp34f67fdjxmdj8w";
      }) {};

    };
}
