let
  pkgs = import ./default.nix;
in {
  nixtodo-api        = pkgs.haskellPackages.nixtodo-api;
  nixtodo-api-client = pkgs.haskell.packages.ghcjsHEAD.nixtodo-api-client;
  nixtodo-backend    = pkgs.haskellPackages.nixtodo-backend;
  nixtodo-frontend   = pkgs.haskell.packages.ghcjsHEAD.nixtodo-frontend;
}
