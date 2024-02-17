{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pyproject-nix.url = "github:nix-community/pyproject.nix";
  inputs.pyproject-nix.inputs.nixpkgs.follows = "nixpkgs";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    pyproject-nix,
  } @ inputs: let
    nixpkgsFor = nixpkgs.lib.genAttrs flake-utils.lib.defaultSystems (system:
      import nixpkgs {
        inherit system;
      });
  in (flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs {
        inherit system;
      };
    in {
      devShells.default = pkgs.mkShell {
        packages = with pkgs; [
          # NodeJS
          nodejs
          nodePackages.pnpm

          # Python
          # pythonEnvironment
          poetry

          # WASM
          fermyon-spin
          wasmedge
        ];
      };
    }
  ));
}
