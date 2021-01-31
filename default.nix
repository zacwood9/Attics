{ ... }:
with (import ./web/common.nix);

import ./build.nix {
  inherit ihp;
  inherit haskellDeps;
  inherit otherDeps;
  projectPath = ./web;
}
