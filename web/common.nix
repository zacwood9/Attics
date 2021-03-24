rec {
    ihp = builtins.fetchGit {
        url = "https://github.com/zacwood9/ihp.git";
        ref = "jobs-dashboard";
        rev = "7c6bd5c39e59c16d66e5b7037ade8379147874a1";
    };
    # ihp = ./IHP;

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
