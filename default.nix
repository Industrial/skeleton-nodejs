{
  self,
  config,
  lib,
  dream2nix,
  ...
}: {
  imports = [
    dream2nix.modules.dream2nix.nodejs-package-json-v3
    dream2nix.modules.dream2nix.nodejs-granular-v3
    dream2nix.modules.dream2nix.nodejs-devshell-v3
  ];

  name = "skeleton-nodejs";
  version = "0.1.0";

  deps = {nixpkgs, ...}: {
    inherit (nixpkgs) stdenv fetchFromGitHub mkShell rsync gnugrep nodejs;
    inherit (nixpkgs.nodePackages) pnpm;
  };

  nodejs-granular-v3 = {
    # buildScript = ''
    #   tsc ./app.ts
    #   mv app.js app.js.tmp
    #   echo "#!${config.deps.nodejs}/bin/node" > app.js
    #   cat app.js.tmp >> app.js
    #   chmod +x ./app.js
    #   patchShebangs .
    # '';

    # buildScript = ''
    #   patchShebangs .
    # '';
  };

  mkDerivation = {
    src = lib.cleanSource ./.;

    # checkPhase = ''
    #   ./app.ts | ${config.deps.gnugrep}/bin/grep -q "Hello, World!"
    # '';
    # doCheck = true;

    # buildPhase = ''
    #   mv app.ts app.ts.tmp
    #   echo "#!${config.deps.deno}/bin/deno" > app.ts
    #   cat app.ts.tmp >> app.ts
    #   chmod +x ./app.ts
    #   patchShebangs .
    # '';

    # tsc ./app.ts
    buildPhase = "mkdir $out";
  };
}
