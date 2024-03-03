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
      pythonPackages = pkgs.python311Packages;

      openai = pythonPackages.buildPythonPackage {
        pname = "openai";
        version = "1.13.3";
        pyproject = true;
        doCheck = false;
        src = pythonPackages.fetchPypi {
          pname = "openai";
          version = "1.13.3";
          sha256 = "sha256-/2xrO8cyfnFeSzWSqSOlocdRn/XddkqD1p9jPUnnens=";
        };
        buildInputs = with pythonPackages; [
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

      flaml = pythonPackages.buildPythonPackage {
        pname = "FLAML";
        version = "2.1.1";
        pyproject = true;
        doCheck = false;
        src = pythonPackages.fetchPypi {
          pname = "FLAML";
          version = "2.1.1";
          sha256 = "sha256-U+lKrMmW2oD+d5vGgz07JcgMd/4RZn0JEnmOSSkygus=";
        };
        buildInputs = with pythonPackages; [
          setuptools
          numpy
        ];
      };

      pyautogen = pythonPackages.buildPythonPackage {
        pname = "pyautogen";
        version = "0.2.14";
        pyproject = true;
        doCheck = false;
        src = pythonPackages.fetchPypi {
          pname = "pyautogen";
          version = "0.2.14";
          sha256 = "sha256-lpMfGOnwg3zlPlKRi8nIFdm9JGONQwyQQGWE0mpWG2g=";
        };
        buildInputs = with pythonPackages; [
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

      python =
        pkgs
        .python311
        .override {
          packageOverrides = self: super: {
            openai = openai;
            flaml = flaml;
            pyautogen = pyautogen;
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

      pythonEnvironment = python.withPackages projectPythonPackages;

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
