{pkgs, ...}: {
  dotenv = {
    enable = true;
  };

  # # https://devenv.sh/basics/
  # env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs;
    [
      git
      redis
      qdrant
    ]
    ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
      # For OSX, use `brew install ta-lib`
      pkgs.ta-lib
    ];

  languages = {
    python = {
      enable = true;
      package = pkgs.python312;
      poetry = {
        enable = true;
        activate = {
          enable = true;
        };
        install = {
          enable = true;
          compile = true;
        };
      };
    };
    javascript = {
      enable = true;
      bun = {
        enable = true;
        install = {
          enable = true;
        };
      };
    };
    typescript = {
      enable = true;
    };
    rust = {
      enable = true;
      channel = "nightly";
      components = ["rustc" "cargo" "clippy" "rustfmt" "rust-analyzer"];
      targets = ["wasm32-unknown-unknown"];
      # toolchain = pkgs;
    };
  };

  # https://devenv.sh/processes/
  # processes.cargo-watch.exec = "cargo-watch";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # # https://devenv.sh/scripts/
  # scripts.hello.exec = ''
  #   echo hello from $GREET
  # '';

  # enterShell = ''
  #   hello
  #   git --version
  # '';

  # # https://devenv.sh/tasks/
  tasks = {
    "bun:lint".exec = "bun run lint";
    "bun:test".exec = "bun run test";

    # "bin:install-local".exec = "bin/install-local";
    # "bash:shellHook" = {
    #   exec = "LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/";
    #   before = ["devenv:enterShell" "devenv:enterTest"];
    # };
    # "devenv:enterShell" = {
    #   after = ["bin:install-local"];
    # };
  };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "> Running tests"
    bun test
  '';

  pre-commit = {
    hooks = {
      # Custom NX hooks
      nx-lint-affected = {
        enable = true;
        name = "nx-lint-affected";
        description = "Run NX lint on affected projects";
        entry = "bun nx affected --target nx-lint";
        pass_filenames = false;
        stages = ["pre-commit"];
      };
      nx-test-affected = {
        enable = true;
        name = "nx-test-affected";
        description = "Run NX tests on affected projects";
        entry = "bun nx affected --target nx-test";
        pass_filenames = false;
        stages = ["pre-push"];
      };

      # Nix
      alejandra = {
        enable = true;
      };
      deadnix = {
        enable = true;
      };
      flake-checker = {
        enable = true;
      };

      # Bash
      shellcheck = {
        enable = false;
      };
      beautysh = {
        enable = true;
      };

      # Markdown
      markdownlint = {
        enable = false;
      };

      # YAML
      check-yaml = {
        enable = false;
      };
      yamllint = {
        enable = true;
        settings = {
          configData = ''
            {
              extends: default,
              ignore: [
                pnpm-lock.yaml
              ],
              rules: {
                line-length: disable
              }
            }
          '';
        };
      };

      # TOML
      check-toml = {
        enable = true;
      };
      taplo = {
        enable = true;
      };

      # JSON
      check-json = {
        enable = true;
      };
      pretty-format-json = {
        enable = false;
      };

      # Git
      check-merge-conflicts = {
        enable = true;
      };
      commitizen = {
        enable = true;
        stages = ["commit-msg"];
      };

      # TypeScript
      biome = {
        enable = true;
      };

      # Rust
      rustfmt = {
        enable = false;
      };

      # Generic
      check-added-large-files = {
        enable = true;
      };
      check-case-conflicts = {
        enable = true;
      };
      check-executables-have-shebangs = {
        enable = true;
      };
      check-shebang-scripts-are-executable = {
        enable = true;
      };
      check-symlinks = {
        enable = true;
      };
      detect-aws-credentials = {
        enable = true;
      };
      detect-private-keys = {
        enable = true;
      };
      end-of-file-fixer = {
        enable = true;
      };
      fix-byte-order-marker = {
        enable = true;
      };
      forbid-new-submodules = {
        enable = true;
      };
      trim-trailing-whitespace = {
        enable = true;
      };
    };
  };

  # See full reference at https://devenv.sh/reference/options/
}
