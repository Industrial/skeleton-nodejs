{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    for-all-systems.url = "github:Industrial/for-all-systems";
    for-all-systems.inputs.nixpkgs.follows = "nixpkgs";
    flake-devshells.url = "github:Industrial/flake-devshells";
    flake-devshells.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs @ {...}: let
    systems = ["x86_64-linux" "aarch64-darwin"];
    forAllSystems = inputs.for-all-systems.forAllSystems {
      inherit systems;
      nixpkgs = inputs.nixpkgs;
    };
  in {
    devShells = forAllSystems ({pkgs, ...}:
      inputs.flake-devshells.devshells {
        packages = with pkgs; [
          # git
          bun
          # deno
          # openssl
          purescript
          spago
          stack
          esbuild
        ];
      });
  };
}
