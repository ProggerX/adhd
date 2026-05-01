{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = {
    flake-parts,
    nixpkgs,
    self,
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.platforms.unix;
      perSystem = {pkgs, ...}: let
        hpkgs = pkgs.haskellPackages;
        opts = {
          root = ./.;
          source-overrides = {};
        };
        pkg = op': hpkgs.developPackage (opts // op');
      in {
        packages.default = pkg {};
        devShells.default = pkg {
          returnShellEnv = true;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              cabal-install
              haskell-language-server
            ]);
        };
      };
      flake = let
        xPkgs = import nixpkgs {system = "x86_64-linux";};
      in {
        nixosModules.default = {pkgs, ...}: {
          imports = [./nixos];
          services.adhd.package = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
        };
        formatter.x86_64-linux = xPkgs.alejandra;
      };
    };
}
