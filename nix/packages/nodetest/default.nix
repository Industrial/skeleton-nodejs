{
  dream2nix,
  config,
  lib,
  self,
  ...
}: {
  imports = [
    dream2nix.modules.dream2nix.nodejs-package-json-v3
    dream2nix.modules.dream2nix.nodejs-granular-v3
    dream2nix.modules.dream2nix.nodejs-devshell-v3
  ];

  name = "nodetest";
  version = "0.1.0";

  deps = {nixpkgs, ...}: {
    inherit (nixpkgs) stdenv fetchFromGitHub mkShell rsync gnugrep nodejs;
    inherit (nixpkgs.nodePackages) pnpm;
  };

  nodejs-granular-v3 = {
    buildScript = ''
      tsc ./app.ts
      mv app.js app.js.tmp
      echo "#!${config.deps.nodejs}/bin/node" > app.js
      cat app.js.tmp >> app.js
      chmod +x ./app.js
      patchShebangs .
    '';
  };

  mkDerivation = {
    src = lib.cleanSource ./.;

    checkPhase = ''
      ./app.js | ${config.deps.gnugrep}/bin/grep -q "Hello, World!"
    '';
    doCheck = true;

    # buildPhase = "mkdir $out";
  };
}
