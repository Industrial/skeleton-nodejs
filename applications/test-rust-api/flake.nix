{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
      inputs.rust-analyzer-src.follows = "";
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
    };
    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    crane,
    fenix,
    flake-utils,
    advisory-db,
    rust-overlay,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      name = "_template-rust";
      target = "wasm32-wasip2";

      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };

      craneLib = (crane.mkLib pkgs).overrideToolchain (p:
        p.rust-bin.selectLatestNightlyWith (toolchain:
          toolchain.default.override {
            targets = [
              target
              "x86_64-unknown-linux-gnu"
              "aarch64-unknown-linux-gnu"
              "aarch64-apple-darwin"
              # "wasm32-unknown-unknown"
            ];
          }));

      src = craneLib.cleanCargoSource ./.;

      commonArgs = {
        inherit src;
        strictDeps = true;
        doCheck = false;
        # cargoExtraArgs = "--target ${target}";
        buildInputs =
          []
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.libiconv
            pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
            pkgs.darwin.apple_sdk.frameworks.CoreFoundation
          ];
      };

      # TODO: Understand why we are creating a LLVM version.
      craneLibLLvmTools =
        craneLib.overrideToolchain
        (fenix.packages.${system}.complete.withComponents [
          "cargo"
          "llvm-tools"
          "rustc"
        ]);

      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      my-crate = craneLib.buildPackage (commonArgs
        // {
          inherit cargoArtifacts;
        });
    in
    {
      checks = {
        inherit my-crate;

        my-crate-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

        # TODO: Fix this. I got errors because 'target/doc' didn't exist.
        # my-crate-doc = craneLib.cargoDoc (commonArgs
        #   // {
        #     inherit cargoArtifacts;
        #   });

        my-crate-fmt = craneLib.cargoFmt {
          inherit src;
        };

        # my-crate-toml-fmt = craneLib.taploFmt {
        #   src = pkgs.lib.sources.sourceFilesBySuffices src [".toml"];
        # };

        # my-crate-audit = craneLib.cargoAudit {
        #   inherit src advisory-db;
        # };

        my-crate-deny = craneLib.cargoDeny {
          inherit src;
        };

        my-crate-nextest = craneLib.cargoNextest (commonArgs
          // {
            inherit cargoArtifacts;
            partitions = 1;
            partitionType = "count";
          });
      };

      packages =
        {
          default = my-crate;
        }
        // pkgs.lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
          # TODO: Understand why we are creating a LLVM version.
          my-crate-llvm-coverage = craneLibLLvmTools.cargoLlvmCov (commonArgs
            // {
              inherit cargoArtifacts;
            });
        };

      apps.default = flake-utils.lib.mkApp {
        # drv = pkgs.writeShellScriptBin "${name}" ''
        #   ${pkgs.wasmtime}/bin/wasmtime run ${my-crate}/bin/${name}.wasm
        # '';
        drv = pkgs.writeShellScriptBin "${name}" ''
          cargo shuttle run
        '';
      };

      devShells = {
        ci = craneLib.devShell {
          checks = self.checks.${system};
          packages = with pkgs; [
            cargo-shuttle
          ];
        };

        default = craneLib.devShell {
          checks = self.checks.${system};
          packages = with pkgs; [
            # Rust
            rustfmt
            leptosfmt

            # Leptos
            cargo-generate
            cargo-leptos
            cargo-shuttle
            sass

            # Development Server
            trunk

            # Wasmtime
            wasmtime
          ];
        };
      };

    });
}
