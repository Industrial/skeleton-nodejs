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
  };

  outputs = inputs @ {self, ...}: let
    forAllSystems = inputs.for-all-systems.forAllSystems {nixpkgs = inputs.nixpkgs;};
  in {
    githubActions = inputs.flake-github-actions.github-actions {
      systems = ["x86_64-linux" "aarch64-darwin"];
      checks = inputs.flake-checks.checks;
    } {inherit inputs;};

    checks = forAllSystems ({system, ...}:
      inputs.flake-checks.checks {
        inherit inputs system;
        run-unit-tests = false;
        custom-hooks = {
          pre-commit = {
            enable = true;
            name = "Pre Push";
            entry = "bun test && bun build";
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
          bun
          direnv
          jq
          pre-commit
          ghc
          haskell.compiler.ghcjs
          cabal-install
        ];
      } {inherit self system pkgs;});
  };
}
