{

  description = "Nix flake for Que-Lambda development";

  inputs = {
    pkgs = {
      url = github:NixOS/nixpkgs;
    };
  };

  outputs =
    { self
    , pkgs
    }:
    {
      devShells.default =
        let
          ghcName = "ghc942";
        in
        pkgs.mkShell {
          buildInputs = [
            pkgs.stdenv
            pkgs.jq
            pkgs.cabal2nix

            pkgs.haskell.compiler.${ghcName}
            pkgs.haskell.packages.${ghcName}.cabal-install
            pkgs.haskell.packages.${ghcName}.ghcid
            pkgs.haskell.packages.${ghcName}.haskell-language-server
            pkgs.haskell.packages.${ghcName}.hlint
            pkgs.haskell.packages.${ghcName}.hoogle
            pkgs.haskell.packages.${ghcName}.hspec-discover
            pkgs.haskell.packages.${ghcName}.ormolu
          ];
        }
    }
}
