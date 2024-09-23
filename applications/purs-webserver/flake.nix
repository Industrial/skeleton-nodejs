{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    for-all-systems.url = "github:Industrial/for-all-systems";
    for-all-systems.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ {...}: let
    systems = ["x86_64-linux" "aarch64-darwin"];
    forAllSystems = inputs.for-all-systems.forAllSystems {
      inherit systems;
      nixpkgs = inputs.nixpkgs;
    };
  in {
    devShells = forAllSystems ({pkgs, ...}: {
      default = pkgs.mkShell {
        packages = with pkgs; [
          bun
          # Had to use PureScript and Spago from package.json
          #purescript
          #spago
          stack
          esbuild
        ];
      };
    });
  };
}
