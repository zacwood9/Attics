let
    compiler = "ghc8103";
    projectPath = ./.;
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.12.0";
    };
    pkgs = import "${toString projectPath}/Config/nix/nixpkgs-config.nix" { inherit ihp compiler; };

    ghc = pkgs.haskell.packages.${compiler};
    haskellPackages = ghc.ghcWithPackages (p: with p; [
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
        haskell-language-server
    ]);
    nativePackages = with pkgs; [
        postgresql
    ];

    make = file: "make -f ${ihp}/lib/IHP/Makefile.dist -B ${file}";
    buildScript = name: "make -f ${ihp}/lib/IHP/Makefile.dist -B build/bin/Script/${name}";
in
  pkgs.stdenv.mkDerivation {
      name = "attics";
      src = projectPath;
      buildInputs = builtins.concatLists [[haskellPackages] nativePackages];
      buildPhase = ''
        ${make "build/bin/RunUnoptimizedProdServer"}
        ${make "build/bin/RunJobs"}
      '';
      installPhase = ''
        mkdir -p $out
        cp -r build/bin $out/bin

        mkdir -p $out/static
        cp -r ./static $out

        mkdir -p $out/Config
        cp -r ./Config $out

        cp -r ${ihp} $out/IHP
      '';
      dontFixup = true;
  }
