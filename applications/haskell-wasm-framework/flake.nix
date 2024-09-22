{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    ghc-wasm-meta = {
      url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = inputs @ {self, ...}: let
    packageName = "haskell-wasm-framework";
    systems = ["x86_64-linux" "aarch64-darwin"];
    forAllSystems = f: inputs.nixpkgs.lib.genAttrs systems (system:
      f {
        inherit system;
        pkgs = import inputs.nixpkgs {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
      });
  in {
    # packages = forAllSystems ({ system, pkgs, ... }: {
    #   "${packageName}" = pkgs.haskellPackages.callCabal2nix packageName self {
    #     # Dependency overrides go here
    #   };
    # });
    # packages.default = self.packages.${system}.${packageName};
    # defaultPackage = self.packages.${system}.default;

    devShells = forAllSystems ({
      system,
      pkgs,
      ...
    }: {
      default = pkgs.mkShell {
        # inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
        packages = with pkgs; [
          # Repository
          direnv

          # TypeScript
          bun

          # GHC with WASM
          haskell.compiler.ghc910
          inputs.ghc-wasm-meta.packages.${system}.all_9_10

          # Haskell
          haskellPackages.cabal-fmt
          haskellPackages.cabal-gild
          haskellPackages.fourmolu
          haskellPackages.haskell-language-server
          haskellPackages.hlint
          haskellPackages.hspec-discover
          ghcid
          cabal-install

          # WASM Tools
          wabt
          wasmtime
        ];
      };
    });
  };
}
