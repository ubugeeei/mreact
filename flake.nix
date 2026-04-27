{
  description = "MReact development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
        };
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          haskellPackages = pkgs.haskell.packages.ghc96;
        in
        {
          default = pkgs.mkShell {
            packages = [
              haskellPackages.ghc
              pkgs.cabal-install
            ];
          };
        }
      );

      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          haskellPackages = pkgs.haskell.packages.ghc96;
        in
        {
          default = haskellPackages.callCabal2nix "mreact" self { };
        }
      );
    };
}
