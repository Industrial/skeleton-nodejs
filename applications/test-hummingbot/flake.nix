{
  description = "Application packaged using poetry2nix";

  inputs = {
    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    poetry2nix,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        myapp = {poetry2nix}:
          poetry2nix.mkPoetryApplication {
            projectDir = self;
            overrides = poetry2nix.overrides.withDefaults (
              final: super: let
                addCommonDeps = name:
                  super.${name}.overridePythonAttrs (old: {
                    nativeBuildInputs =
                      (old.nativeBuildInputs or [])
                      ++ [
                        final.hatchling
                        final.poetry-core
                        final.scikit-build
                        final.setuptools
                        final.wheel
                      ];
                    buildInputs = (old.buildInputs or []) ++ [final.scikit-build];
                  });
              in {
                asyncio = addCommonDeps "asyncio";
                coincurve = addCommonDeps "coincurve";
                dataclassy = addCommonDeps "dataclassy";
                hdwallets = addCommonDeps "hdwallets";
                hummingbot = addCommonDeps "hummingbot";
                mnemonic = addCommonDeps "mnemonic";
                pyunormalize = addCommonDeps "pyunormalize";
                safe-pysha3 = addCommonDeps "safe-pysha3";
                # Make pysha3 the safe version.
                pysha3 = super.safe-pysha3.overridePythonAttrs (old: {
                  nativeBuildInputs = (old.nativeBuildInputs or []) ++ [final.poetry-core final.setuptools final.wheel];
                });
              }
            );
          };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            poetry2nix.overlays.default
            (final: _: {
              myapp = final.callPackage myapp {};
              # pysha3 = nixpkgs.development.python-modules.safe-pysha3;
            })
          ];
        };
      in {
        packages.default = pkgs.myapp;
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [pkgs.myapp];
            packages = with pkgs; [poetry];
          };
          poetry = pkgs.mkShell {
            packages = [pkgs.poetry];
          };
        };
        legacyPackages = pkgs;
      }
    );
}
