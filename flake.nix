{

  description = "Nix flake for Que-Lambda development";

  inputs = {
    flake-utils = {
      url = github:numtide/flake-utils;
    };
    nixpkgs = {
      url = github:NixOS/nixpkgs;
    };
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    }:
    flake-utils.lib.eachDefaultSystem (system:
    {
      devShells.default =
        let
          pkgs = import nixpkgs { inherit system; };
          ghcName = "ghc942";
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.stdenv
            pkgs.jq
            pkgs.cabal2nix

            pkgs.haskell.compiler.${ghcName}
            pkgs.haskell.packages.${ghcName}.cabal-install
            # pkgs.haskell.packages.${ghcName}.ghcid
            pkgs.haskell.packages.${ghcName}.haskell-language-server
            # pkgs.haskell.packages.${ghcName}.hlint
            # pkgs.haskell.packages.${ghcName}.hoogle
            #pkgs.haskell.packages.${ghcName}.hspec-discover
            # pkgs.haskell.packages.${ghcName}.ormolu
          ];
        };
    });
}
