{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    for-all-systems.url = "github:Industrial/for-all-systems";
    for-all-systems.inputs.nixpkgs.follows = "nixpkgs";
    flake-checks.url = "github:Industrial/flake-checks/v1.1.0";
    flake-checks.inputs.nixpkgs.follows = "nixpkgs";
    flake-devshells.url = "github:Industrial/flake-devshells";
    flake-devshells.inputs.nixpkgs.follows = "nixpkgs";
    flake-github-actions.url = "github:Industrial/flake-github-actions";
    flake-github-actions.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
    nix-github-actions.url = "github:nix-community/nix-github-actions";
    nix-github-actions.inputs.nixpkgs.follows = "nixpkgs";
    # ghc-wasm-meta.url = "gitlab:ghc/ghc-wasm-meta?host=gitlab.haskell.org";
    ghc-wasm-meta.url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
  };

  outputs = inputs @ {self, ...}: let
    systems = ["x86_64-linux" "aarch64-darwin"];
    forAllSystems = inputs.for-all-systems.forAllSystems {
      inherit systems;
      nixpkgs = inputs.nixpkgs;
    };
  in {
    githubActions = inputs.flake-github-actions.github-actions {
      inherit systems;
      checks = inputs.flake-checks.checks;
    } {inherit inputs;};

    checks = forAllSystems ({system, ...}:
      inputs.flake-checks.checks {
        inherit inputs system;
        run-unit-tests = false;
        custom-hooks = {
          pre-commit = {
            enable = true;
            name = "Pre Commit";
            entry = "bun lint && bun test && bun build";
            pass_filenames = false;
            stages = ["pre-commit"];
          };
        };
      });

    devShells = forAllSystems ({
      system,
      pkgs,
      ...
    }:
      inputs.flake-devshells.devshells {
        packages = with pkgs; [
          # Repository
          (
            if system == "x86_64-linux"
            then inotify-tools
            else null
          )
          # inotify-tools
          pre-commit
          direnv

          # TypeScript
          bun
          nodePackages.rollup
          jq

          # EdgeDB
          edgedb

          # Ollama
          ollama

          # Haskell & WASM
          # haskell.compiler.ghcjs
          # haskell.packages.ghcjs.ghcjs-base
          # haskellPackages.base_4_20_0_1
          # haskellPackages.ghc-experimental
          # haskellPackages.ghcjs-base
          # haskellPackages.ghcjs-fetch

          # Cabal
          cabal-install

          # GHC with WASM
          haskell.compiler.ghc910
          inputs.ghc-wasm-meta.packages.${system}.all_9_10

          # Haskell Packages
          haskell-language-server
          haskellPackages.cabal-fmt
          haskellPackages.cabal-gild
          haskellPackages.fourmolu
          haskellPackages.hlint

          # WASM Tools
          wabt
          wasmtime
        ];
      } {inherit self system pkgs;});
  };
}
