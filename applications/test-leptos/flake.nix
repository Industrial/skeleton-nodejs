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
      # Common arguments can be set here to avoid repeating them later
      commonArgs = {
        inherit src;
        strictDeps = true;
        # Tests currently need to be run via `cargo wasi` which
        # isn't packaged in nixpkgs yet...
        doCheck = false;
        cargoExtraArgs = "--target wasm32-wasi";
        buildInputs =
          [
            # Add additional build inputs here
          ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
            # Additional darwin specific inputs can be set here
            pkgs.libiconv
          ];
        # Additional environment variables can be set directly
        # MY_CUSTOM_VAR = "some value";
      };

      # TODO: Understand why we are creating a LLVM version.
      craneLibLLvmTools =
        craneLib.overrideToolchain
        (fenix.packages.${system}.complete.withComponents [
          "cargo"
          "llvm-tools"
          "rustc"
        ]);

      # Build *just* the cargo dependencies, so we can reuse
      # all of that work (e.g. via cachix) when running in CI
      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      # Build the actual crate itself, reusing the dependency
      # artifacts from above.
      my-crate = craneLib.buildPackage (commonArgs
        // {
          inherit cargoArtifacts;
        });
    in {
      checks = {
        # Build the crate as part of `nix flake check` for convenience
        inherit my-crate;
        # Run clippy (and deny all warnings) on the crate source,
        # again, reusing the dependency artifacts from above.
        #
        # Note that this is done as a separate derivation so that
        # we can block the CI if there are issues here, but not
        # prevent downstream consumers from building our crate by itself.
        my-crate-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });
        my-crate-doc = craneLib.cargoDoc (commonArgs
          // {
            inherit cargoArtifacts;
          });
        # Check formatting
        my-crate-fmt = craneLib.cargoFmt {
          inherit src;
        };
        my-crate-toml-fmt = craneLib.taploFmt {
          src = pkgs.lib.sources.sourceFilesBySuffices src [".toml"];
          # taplo arguments can be further customized below as needed
          # taploExtraArgs = "--config ./taplo.toml";
        };
        # Audit dependencies
        my-crate-audit = craneLib.cargoAudit {
          inherit src advisory-db;
        };
        # Audit licenses
        my-crate-deny = craneLib.cargoDeny {
          inherit src;
        };
        # Run tests with cargo-nextest
        # Consider setting `doCheck = false` on `my-crate` if you do not want
        # the tests to run twice
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
      # apps.default = flake-utils.lib.mkApp {
      #   drv = my-crate;
      # };
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
