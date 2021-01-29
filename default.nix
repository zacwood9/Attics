{ ... }:

let
  ihp = builtins.fetchGit {
      url = "https://github.com/zacwood9/ihp.git";
      branchName = "ihp-logging";
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
