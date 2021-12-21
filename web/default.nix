let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.17.0";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
          cabal-install
          base
          wai
          text
          hlint
          p.ihp
          csv
          raw-strings-qq
          minio-hs
          tz
          hspec
        ];
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv.overrideAttrs (oldAttrs: {
        installPhase = oldAttrs.installPhase + ''
            ln -s "$out/lib/build/ihp-lib" "$out/IHP"
            ln -s "$out/lib/static" "$out/static"
        '';
    })
