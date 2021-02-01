rec {
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.8.0";
    };

    haskellDeps = p: with p; [
      cabal-install
      base
      wai
      text
      hlint
      p.ihp
      hspec
      raw-strings-qq
      ip
    ];

    otherDeps = p: with p; [
        # Native dependencies, e.g. imagemagick
    ];

    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        inherit ihp;
        inherit haskellDeps;
        inherit otherDeps;
        projectPath = ./.;
    };
}
