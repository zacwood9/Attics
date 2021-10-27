{ ihp, compiler }:
import "${toString ihp}/NixSupport/make-nixpkgs-from-options.nix" {
    inherit ihp compiler;
    haskellPackagesDir = ./haskell-packages/.;
}
