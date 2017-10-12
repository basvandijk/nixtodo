# This Nix expression defines an overlay which is a way to extend or override
# the packages in <nixpkgs> with your own packages.
#
# For more information about overlays see:
# https://nixos.org/nixpkgs/manual/#chap-overlays
#
# The first argument `self` corresponds to the final package set. You should use
# this set for the dependencies of all packages specified in your overlay.
#
# The second argument `super` corresponds to the result of the evaluation of the
# previous stages of Nixpkgs. It does not contain any of the packages added by
# the current overlay, nor any of the following overlays. This set should be
# used either to refer to packages you wish to override, or to access functions
# defined in Nixpkgs.

self: super:

let
  # Note that we extend both the `haskellPackages` set as well as
  # `haskell.packages.ghcjsHEAD` set with the same overrides. That's why we give
  # it a name here.
  haskellOverrides = import ./haskell-overrides.nix self;
in

{
  haskellPackages = super.haskellPackages.override haskellOverrides;

  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcjsHEAD = super.haskell.packages.ghcjsHEAD.override haskellOverrides;
    };
  };

  # Here we're extending <nixpkgs> with the attribute `nixtodo.frontend-static`.
  # This is a derivation which applies the closure-compiler to the
  # `nixtodo-frontend` GHCJS package to compress and optimize it. This is the
  # package that we're actually going to deploy to production.
  nixtodo = {
    frontend-static =
      let pkg = self.haskell.packages.ghcjsHEAD.nixtodo-frontend;
      in self.runCommand "${pkg.pname}-static" rec {
           indexTemplate = "${pkg}/index.html.mustache";
           static        = "${pkg}/static";
           jsFiles       = "all.js";
           optimized = self.stdenv.mkDerivation rec {
             inherit jsFiles pkg;

             name = "${pkg.pname}-optimized-${pkg.version}";
             dir = "bin/${pkg.pname}.jsexe";

             buildInputs = [ self.closurecompiler ];
             buildCommand = ''
               source $stdenv/setup

               mkdir -p "$out/$dir"

               for jsFile in ''${jsFiles[@]}; do
                 closure-compiler "$pkg/$dir/$jsFile" \
                   --warning_level=QUIET \
                   --compilation_level=SIMPLE_OPTIMIZATIONS \
                   > "$out/$dir/$jsFile"
               done
             '';
           };
         } ''
             mkdir -p $out/static/js

             cp $indexTemplate $out/index.html.mustache

             for jsFile in ''${jsFiles[@]}; do
               cp "$optimized/bin/${pkg.pname}.jsexe/$jsFile" "$out/static/js/$jsFile"
             done

             cp -r $static/* $out/static/ # */
         '';
  };
}
