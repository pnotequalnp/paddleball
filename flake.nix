{
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hs = pkgs.haskellPackages;
        lib = pkgs.haskell.lib;
        yampa-gloss = lib.doJailbreak (lib.markUnbroken hs.yampa-gloss);
      in
      rec {
        packages.paddleball = hs.callCabal2nix "paddleball" ./. { inherit yampa-gloss; };
        defaultPackage = packages.paddleball;

        apps.paddleball = flake-utils.lib.mkApp { drv = packages.paddleball; };
        defaultApp = apps.paddleball;

        devShell = packages.paddleball.env.overrideAttrs
          (super: {
            buildInputs = with hs; super.buildInputs ++ [
              cabal-install
              fourmolu
              haskell-language-server
              hlint
            ];
          });
      });
}
