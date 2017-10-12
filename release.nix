let
  pkgs = import ./default.nix;
in {
  nixtodo-api        = pkgs.haskellPackages.nixtodo-api;
  nixtodo-api-client = pkgs.haskell.ghcjsHEAD.nixtodo-api-client;
  nixtodo-backend    = pkgs.haskellPackages.nixtodo-backend;
  nixtodo-frontend   = pkgs.haskell.ghcjsHEAD.nixtodo-frontend;
}
