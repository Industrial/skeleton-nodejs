{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = {
    self,
    nixpkgs,
    flake-utils,
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
        buildInputs = with pkgs; [
          nodejs
          nodePackages.pnpm

          python310
          poetry
        ];
      };
    }
  ));
}
