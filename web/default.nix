let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.17.0";
    };
    haskellLib = pkgs.haskell.lib;
    manualOverrides = haskellPackagesNew: haskellPackagesOld:
      {
      };

    additionalNixpkgsOptions = { allowUnfree = true; };

    pkgs = import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
        inherit ihp manualOverrides additionalNixpkgsOptions;
        haskellPackagesDir = ./Config/nix/haskell-packages/.;
    };

    haskellPackages = pkgs.haskell.packages.ghc8107;
    package = isProd:
      (haskellPackages.callCabal2nixWithOptions
        "App" # cabal file name
        ./.   # source directory
        (if isProd then "--flag Prod" else "")  # cabal flags
        {} # additional options
      ).overrideAttrs (oldAttrs: {
        preBuild = (if (builtins.hasAttr "preBuild" oldAttrs) then oldAttrs.preBuild else "") + "${haskellPackages.ihp}/bin/build-generated-code";
        installPhase = oldAttrs.installPhase + ''
          mkdir -p $out/IHP $out/static
          cp -r $src/static $out
          cp -r ${haskellPackages.ihp}/lib/IHP/static $out/IHP
          '';
      });
in
  if pkgs.lib.inNixShell
    then
      haskellPackages.shellFor {
        packages = p: [
          (package false)
        ];
        buildInputs = with haskellPackages; [
          pkgs.cabal-install
          pkgs.postgresql

          ihp
        ];
        withHoogle = true;
      }
    else
      (package true)
