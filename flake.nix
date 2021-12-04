{
  description = "aoc";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    treefmt.url = "github:numtide/treefmt";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt, nix-filter, ... }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;
            name = "aoc";
            root = nix-filter.lib.filter {
              root = ./.;
              include = [
                "aoc.cabal"
                (nix-filter.lib.inDirectory "src")
                (nix-filter.lib.inDirectory "inputs")
              ];
            };
            withHoogle = true;
            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              if returnShellEnv
              then [
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                fourmolu
                pkgs.nixpkgs-fmt
                treefmt.defaultPackage.${system}
              ]
              else [ ]
              );
          };
      in
      {
        packages = {
          aoc = project false;
        };
        defaultPackage = self.packages.${system}.aoc;
        devShell = project true;
      }
    )
  ;
}
