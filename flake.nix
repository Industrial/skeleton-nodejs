{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };
  outputs = inputs @ {
    self,
    flake-parts,
    nixpkgs,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = with inputs; [
        flake-root.flakeModule
        mission-control.flakeModule
        pre-commit-hooks.flakeModule
      ];
      flake = {};
      perSystem = {
        system,
        pkgs,
        config,
        ...
      }: {
        mission-control.scripts = {
          flake-update = {
            description = "Update flake inputs";
            exec = "nix flake update --recreate-lock-file 2>&1 | tee logs/flake-update.log";
          };
          test = {
            description = "Run unit tests with coverage.";
            exec = "bun test --coverage 2>&1 | tee logs/test.log";
          };
          update = {
            description = "Update package versions";
            exec = "bunx npm-check-updates -p pnpm -u 2>&1 | tee logs/update.log && pnpm i 2>&1 | tee logs/install.log";
          };
          lint = {
            description = "Run eslint and fix issues.";
            exec = "bunx tsc -p . --noEmit 2>&1 | tee logs/tsc.log && bunx eslint --fix --cache . 2>&1 | tee logs/eslint.log";
          };
        };
        checks = {
          pre-commit-check = inputs.pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              alejandra.enable = true;
              check-added-large-files.enable = true;
              check-case-conflicts.enable = true;
              check-executables-have-shebangs.enable = true;
              check-json.enable = true;
              check-merge-conflicts.enable = true;
              check-shebang-scripts-are-executable.enable = true;
              check-symlinks.enable = true;
              check-toml.enable = true;
              check-yaml.enable = true;
              commitizen = {
                enable = true;
                stages = ["commit-msg"];
              };
              detect-aws-credentials.enable = true;
              detect-private-keys.enable = true;
              end-of-file-fixer.enable = true;
              eslint.enable = true;
              fix-byte-order-marker.enable = true;
              flake-checker.enable = true;
              forbid-new-submodules.enable = true;
              markdownlint.enable = true;
              shellcheck.enable = true;
              trim-trailing-whitespace.enable = true;
            };
          };
        };
        devShells.default = pkgs.mkShell {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          buildInputs = self.checks.${system}.pre-commit-check.enabledPackages;
          inputsFrom = [
            config.mission-control.devShell
          ];
          packages = with pkgs; [
            alejandra
            bun
          ];
        };
      };
    };
}
