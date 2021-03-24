rec {
    ihp = builtins.fetchGit {
        url = "https://github.com/zacwood9/ihp.git";
        ref = "jobs-dashboard";
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
    ];

    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        inherit ihp;
        inherit haskellDeps;
        inherit otherDeps;
        projectPath = ./.;
    };
}
