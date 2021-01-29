let
    ihp = builtins.fetchGit {
        url = "https://github.com/zacwood9/ihp.git";
        branchName = "ihp-logging";
    };
    # ihp = ./IHP;
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = import ./haskellDeps.nix;
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
