{pkgs ? import <nixpkgs> {} }:
  let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/haskellframework.git";
        ref = "refs/tags/v0.8.0";
    };
    app = import ./build.nix {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./web;
    };
  in
  pkgs.stdenv.mkDerivation rec {
    name = "attics";
    src = ./.;
    buildInputs = [ app ];
    installPhase = ''
      mkdir -p $out
      cp -r ${app} $out/web
    '';
  }
