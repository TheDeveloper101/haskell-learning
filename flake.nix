{
  description = "Haskell flake for amerihac";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages."x86_64-linux";
    in
    {
      devShells."x86_64-linux".default = pkgs.mkShell {
        packages = with pkgs; [
          haskell-ci
          hlint
          haskellPackages.hoogle
          haskellPackages.cabal-install
          gcc
          gmp
          haskell.compiler.ghc96
        ];
      };
    };
}
