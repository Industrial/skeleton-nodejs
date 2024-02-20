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

      python = pkgs.python3.override {
        packageOverrides = self: super: {};
      };

      project = pyproject-nix.lib.project.loadPyproject {
        projectRoot = ./.;
      };

      projectPythonPackages = project.renderers.withPackages {inherit python;};

      extraPythonPackages = ps: let
        # scikit-learn = ps.buildPythonPackage {
        #   pname = "scikit-learn";
        #   version = "1.4.0";
        #   src = ps.fetchPypi {
        #     pname = "scikit-learn";
        #     version = "1.4.0";
        #     sha256 = "sha256-1Dc8mE66IOOTIW7dUaPj7t5Wy+k9QkdRbSBWQ8O5MSE=";
        #   };
        #   buildInputs = with ps; [
        #     numpy
        #     pyspark
        #     pandas
        #     scipy
        #     pytest
        #   ];
        #   nativeBuildInputs = with ps; [
        #     cython
        #     pytest
        #   ];
        # };
        flaml = ps.buildPythonPackage {
          pname = "FLAML";
          version = "2.1.1";
          src = ps.fetchPypi {
            pname = "FLAML";
            version = "2.1.1";
            sha256 = "sha256-U+lKrMmW2oD+d5vGgz07JcgMd/4RZn0JEnmOSSkygus=";
          };
          buildInputs = with ps; [
            numpy
            pyspark
            pandas
            scipy
            scikit-learn
            transformers
            tiktoken
          ];
        };

        pyautogen = ps.buildPythonPackage {
          pname = "pyautogen";
          version = "0.2.13";
          src = ps.fetchPypi {
            pname = "pyautogen";
            version = "0.2.13";
            sha256 = "sha256-nsjYER/tEghgw3Lx2WKjPosPwmV8YkMy9AsSBO7B+fk=";
          };
          buildInputs = with ps; [
            pip
            docker
            pydantic
            tiktoken
            python-dotenv
            flaml
          ];
        };
      in [
        ps.numpy
        ps.pandas
        pyautogen
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
          # Python
          pythonEnvironment
          # virtualenv
          # poetry
        ];
      };
    }
  ));
}
