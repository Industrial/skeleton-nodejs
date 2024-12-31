{pkgs, ...}: {
  dotenv = {
    enable = true;
  };

  # # https://devenv.sh/basics/
  # env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs;
    [git]
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
  # tasks = {
  #   "bin:install-local".exec = "bin/install-local";

  #   "bash:shellHook" = {
  #     exec = "LD_LIBRARY_PATH=${pkgs.stdenv.cc.cc.lib}/lib/";
  #     before = ["devenv:enterShell" "devenv:enterTest"];
  #   };

  #   "devenv:enterShell" = {
  #     after = ["bin:install-local"];
  #   };
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
  '';

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  pre-commit = {
    hooks = {
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
        enable = true;
      };
      beautysh = {
        enable = true;
      };

      # Markdown
      markdownlint = {
        enable = true;
      };

      # YAML
      check-yaml = {
        enable = true;
      };
      yamllint = {
        enable = true;
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
        enable = true;
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
        enable = true;
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
