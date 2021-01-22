{ ... }:

let
  ihp = builtins.fetchGit {
      url = "https://github.com/digitallyinduced/haskellframework.git";
      ref = "refs/tags/v0.8.0";
  };
in
  import ./build.nix {
    ihp = ihp;
    haskellDeps = import ./web/haskellDeps.nix;
    otherDeps = p: with p; [
        # Native dependencies, e.g. imagemagick
    ];
    projectPath = ./web;
  }
