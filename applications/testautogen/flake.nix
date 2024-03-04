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
      pkgs = import nixpkgs {inherit system;};

      python =
        pkgs
        .python311
        .override {
          packageOverrides = self: super: let
            pythonPackages = self;
          in {
            # langchain = self.buildPythonPackage {
            #   pname = "langchain";
            #   version = "0.1.10";
            #   pyproject = true;
            #   doCheck = false;
            #   src = self.fetchPypi {
            #     pname = "langchain";
            #     version = "0.1.10";
            #     sha256 = "sha256-F5UbzW10rcdKoIHyYO9VFMRJSIgVMUQgt+D4NJ8V2TI=";
            #   };
            #   nativeBuildInputs = with self; [
            #     pkgs.poetry
            #   ];
            #   buildInputs = with self; [
            #     pkgs.poetry
            #     # pkgs.poetry
            #   ];
            # };

            openai = self.buildPythonPackage {
              pname = "openai";
              version = "1.13.3";
              pyproject = true;
              doCheck = false;
              src = self.fetchPypi {
                pname = "openai";
                version = "1.13.3";
                sha256 = "sha256-/2xrO8cyfnFeSzWSqSOlocdRn/XddkqD1p9jPUnnens=";
              };
              buildInputs = with self; [
                hatchling
                anyio
                distro
                httpx
                pydantic
                sniffio
                tqdm
                typing-extensions
              ];
            };

            flaml = self.buildPythonPackage {
              pname = "FLAML";
              version = "2.1.1";
              pyproject = true;
              doCheck = false;
              src = self.fetchPypi {
                pname = "FLAML";
                version = "2.1.1";
                sha256 = "sha256-U+lKrMmW2oD+d5vGgz07JcgMd/4RZn0JEnmOSSkygus=";
              };
              buildInputs = with self; [
                setuptools
                numpy
              ];
            };

            pyautogen = self.buildPythonPackage {
              pname = "pyautogen";
              version = "0.2.14";
              pyproject = true;
              doCheck = false;
              src = self.fetchPypi {
                pname = "pyautogen";
                version = "0.2.14";
                sha256 = "sha256-lpMfGOnwg3zlPlKRi8nIFdm9JGONQwyQQGWE0mpWG2g=";
              };
              buildInputs = with self; [
                diskcache
                docker
                flaml
                openai
                pydantic
                python-dotenv
                setuptools
                termcolor
                tiktoken
              ];
            };
          };
        };

      project = pyproject-nix.lib.project.loadPyproject {
        projectRoot = ./.;
      };

      projectPythonPackages = project.renderers.withPackages {
        inherit python;
      };

      projectPythonAttributes = project.renderers.buildPythonPackage {
        inherit python;
      };

      pythonEnvironment = python.withPackages (ps: let
        packages =
          ps
          // {
            chromadb = pkgs.python311Packages.chromadb;
            flaml = pkgs.python311Packages.flaml;
            numpy = pkgs.python311Packages.numpy;
          };
      in
        projectPythonPackages packages);

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

      packages.default = python.pkgs.buildPythonApplication (projectPythonAttributes
        // {
          env.CUSTOM_ENVVAR = "hello";
        });
    }
  ));
}
