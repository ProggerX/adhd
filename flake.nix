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
      opts = {
        root = ./.;
        source-overrides = {};
      };
      pkg = nativePkgs: targetPkgs: op:
        ((targetPkgs.haskell.packages.ghc910.override {
          overrides = _: _: {
            inherit (nativePkgs.haskell.packages.ghc910) cabal2nix;
          };
        }).developPackage (opts // op));
    in {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];
      perSystem = {pkgs, ...}: {
        formatter = pkgs.alejandra;
        packages.default = pkg pkgs pkgs {};
        packages.static = pkg pkgs pkgs.pkgsStatic {
          modifier = drv:
            pkgs.haskell.lib.compose.justStaticExecutables (
              drv.overrideAttrs (old: {
                nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.nukeReferences ];
                allowedReferences = [ "out" ];
                postFixup = (old.postFixup or "") + ''
                  nuke-refs $out/bin/adhd
                  mkdir -p $out/nix-support
                  echo "file binary-dist \"$out/bin/adhd\"" >> $out/nix-support/hydra-build-products
                '';
              })
            );
        };

        devShells.default = pkg pkgs pkgs {
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
        };
      };
    });
}
