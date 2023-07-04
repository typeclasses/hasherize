{ pkgs }:

let
  inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

  hls = pkgs.haskell-language-server.override {
    supportedGhcVersions = [ "96" ];
  };

  combineOverrides = old:
    fold composeExtensions (old.overrides or (_: _: { }));

  haskellPackages =
    let
      inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
    in
    pkgs.haskell.packages.ghc96.override (old: {
      overrides =
        combineOverrides old [
          (packageSourceOverrides { hasherize = ../hasherize; })
          (new: old: {
            file-io = new.callPackage ./haskell/file-io.nix { };
            qsem = new.callPackage ./haskell/qsem.nix { };
          })
        ];
    });

in
{
  packages.default =
    pkgs.haskell.lib.justStaticExecutables
      haskellPackages.hasherize;

  devShells.default = pkgs.mkShell {
    inputsFrom = [ haskellPackages.hasherize.env ];
    buildInputs = [ hls pkgs.cabal-install ];
  };
}
