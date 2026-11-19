{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = {
    flake-parts,
    self,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} (let
      hpkgs = pkgs: pkgs.haskell.packages.ghc910;
      opts = {
        root = ./.;
        source-overrides = {};
      };
      pkg = pkgs: op:
        (hpkgs pkgs).developPackage (opts // op);
    in {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];
      perSystem = {pkgs, ...}: {
        formatter = pkgs.alejandra;
        packages.default = pkg pkgs {};
        packages.static = pkg pkgs.pkgsStatic {};
        packages.cross-aarch64-linux = pkg pkgs.pkgsCross.aarch64-multiplatform {};
        packages.cross-aarch64-linux-static = pkg pkgs.pkgsCross.aarch64-multiplatform.pkgsStatic {};

        devShells.default = pkg pkgs {
          returnShellEnv = true;
          modifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with pkgs; [
              cabal-install
              haskell-language-server
            ]);
        };
      };
      flake = {
        nixosModules.default = {pkgs, ...}: {
          imports = [./nixos];
          services.adhd.package = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
        };
        hydraJobs = {
          packages.x86_64-linux = self.packages.x86_64-linux;
          packages.cross-aarch64-linux.default = self.packages.x86_64-linux.cross-aarch64-linux;
          packages.cross-aarch64-linux.static = self.packages.x86_64-linux.cross-aarch64-linux-static;
        };
      };
    });
}
