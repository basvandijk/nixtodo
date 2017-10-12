# This Nix expression returns the package set from nixpkgs but extended and
# overridden by our own packages as specified by the given overlays.

let
  nixpkgsPath = import ./nixpkgs.nix;
  nixpkgs     = import nixpkgsPath;
in nixpkgs { overlays = [ (import ./nix/overlay.nix) ]; }
