# This expression returns the /nix/store path to our version of nixpkgs.
# It ensures that all engineers use the same revision of nixpkgs.
#
# This technique was inspired by the article:
#
#   Reproducible Development Environments by Rok Garbas
#   https://garbas.si/2015/reproducible-development-environments.html

let # Note that we depend on a <nixpkgs> in our NIX_PATH. This nixpkgs is only
    # used to access the `fetchFromGitHub` function which is used to fetch the
    # desired version of nixpkgs.
    pkgs = import <nixpkgs> {};

    nixpkgs = pkgs.fetchFromGitHub {
       owner   = "NixOS";
       repo    = "nixpkgs";
       # Points to a recent commit on the nixos-17.09-small branch.
       rev     = "3c0ea4fa4b931501212e1cf2708ea67cc3dbbcdf";
       sha256  = "11iwvnbwns4gdfakjr8qxzzcbzb718chlihxrm2fypg91k1kh7ws";
     };

    # Often times you need some modifications to be made to nixpkgs. For example
    # you may have created a Pull Request that makes a change to some NixOS
    # module and it hasn't been merged yet. In those cases you can take the
    # corresponding patch and apply it to the nixpkgs that we've checked out
    # above.
    patches = [
      # # Adds the postage package and NixOS module
      # (pkgs.fetchpatch {
      #   url = "https://github.com/NixOS/nixpkgs/commit/943c78b10d8ed4418dbb6fb9a89e6f416af511d5.patch";
      #   sha256 = "13vhrkihbw7nrwplxlhfvwm493h53y7yzs8j5nsxnhv70hhpiwc4";
      # })
    ];

in pkgs.runCommand ("nixpkgs-" + nixpkgs.rev) {inherit nixpkgs patches; } ''
  cp -r $nixpkgs $out
  chmod -R +w $out
  for p in $patches ; do
    echo "Applying patch $p"
    patch -d $out -p1 < "$p"
  done
''
