rec {
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "master";
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
      fast-logger
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
