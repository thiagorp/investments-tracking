{
  description = "investments-tracking";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      supportedGHCVersion = "902";
      compilerVersion = "ghc${supportedGHCVersion}";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          investments-tracking = hfinal.callCabal2nix "investments-tracking" ./. {};
        };
      };
    in rec {
      packages =
        utils.flattenTree
        {investments-tracking = hsPkgs.investments-tracking;};

      devShell = hsPkgs.shellFor {
        withHoogle = true;
        packages = p: [
          p.investments-tracking
        ];
        buildInputs = with pkgs;
          [
            alejandra
            cabal2nix
            hsPkgs.cabal-install
            hsPkgs.ghcid
            hsPkgs.fourmolu
            hsPkgs.haskell-language-server
          ]
          ++ (builtins.attrValues (import ./scripts.nix {inherit pkgs;}));
      };

      # nix build
      defaultPackage = packages.investments-tracking;
    });
}
