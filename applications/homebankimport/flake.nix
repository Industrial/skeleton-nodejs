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
  } @ inputs: (flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs {
        inherit system;
      };

      python = pkgs.python3.override {
        packageOverrides = self: super: {};
      };

      project = pyproject-nix.lib.project.loadPyproject {
        projectRoot = ./.;
      };

      projectPythonPackages = project.renderers.withPackages {inherit python;};

      projectPythonApplication = project.renderers.buildPythonPackage {inherit python;};

      extraPythonPackages = ps: let
      in [
        # ps.numpy
        # ps.pandas
      ];

      pythonEnvironment = pkgs.python3.withPackages (
        ps:
          (projectPythonPackages ps) ++ (extraPythonPackages ps)
      );

      shellHook = with pkgs; ''
        export LD_LIBRARY_PATH=${stdenv.cc.cc.lib.outPath}/lib/:$LD_LIBRARY_PATH
      '';
    in {
      devShells.default = pkgs.mkShell {
        shellHook = shellHook;
        packages = with pkgs; [
          pythonEnvironment
        ];
      };

      packages.default = python.pkgs.buildPythonPackage (projectPythonApplication
        // {
          env.CUSTOM_ENVVAR = "hello";
        });
    }
  ));
}
