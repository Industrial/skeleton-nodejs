{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    crane.url = "github:ipetkov/crane";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.rust-analyzer-src.follows = "";
    };
    flake-utils.url = "github:numtide/flake-utils";
    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
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
      pkgs = import nixpkgs {
        inherit system;
        overlays = [(import rust-overlay)];
      };

      craneLib = (crane.mkLib pkgs).overrideToolchain (p:
        p.rust-bin.selectLatestNightlyWith (toolchain:
          toolchain.default.override {
            targets = [
              "wasm32-unknown-unknown"
            ];
          }));

      src = craneLib.cleanCargoSource ./.;

      commonArgs = {
        inherit src;
        strictDeps = true;
        doCheck = false;
        cargoExtraArgs = "--target wasm32-unknown-unknown";
        buildInputs =
          [
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            pkgs.libiconv
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
    in {
      checks = {
        inherit my-crate;

        my-crate-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });
        my-crate-doc = craneLib.cargoDoc (commonArgs
          // {
            inherit cargoArtifacts;
          });

        my-crate-fmt = craneLib.cargoFmt {
          inherit src;
        };

        my-crate-toml-fmt = craneLib.taploFmt {
          src = pkgs.lib.sources.sourceFilesBySuffices src [".toml"];
        };

        my-crate-audit = craneLib.cargoAudit {
          inherit src advisory-db;
        };

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
        drv = pkgs.writeShellScriptBin "my-app" ''
          ${pkgs.wasmtime}/bin/wasmtime run ${my-crate}/bin/custom-toolchain.wasm
        '';
      };

      devShells.default = craneLib.devShell {
        # Inherit inputs from checks.
        checks = self.checks.${system};
        # Additional dev-shell environment variables can be set directly
        # MY_CUSTOM_DEVELOPMENT_VAR = "something else";
        # Extra inputs can be added here; cargo and rustc are provided by default.
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
        ];
      };
    });
}
