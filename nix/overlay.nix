self: super:

let
  haskellOverrides = import ./haskell-overrides.nix self;
in

{
  haskellPackages = super.haskellPackages.override haskellOverrides;
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcjsHEAD = super.haskell.packages.ghcjsHEAD.override haskellOverrides;
    };
  };

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
