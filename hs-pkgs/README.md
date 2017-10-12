This directory contains all our Haskell packages.

The Nix attribute `pkgs.haskellPackages` and `pkgs.haskell.packages.ghcjsHEAD`
is automatically extended with all the packages listed in this directory. See
`<nixtodo/nix/overlay.nix>` how this is accomplished.

3rd party packages should be stored under `<nixtodo/nix/haskell>`.
