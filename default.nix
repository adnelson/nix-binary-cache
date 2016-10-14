{
  pkgs ? import <nixpkgs> {config.allowUnfree = true;},
  compiler ? "ghc7102"
}:

import ./project.nix {
  inherit pkgs;
  haskellPackages = pkgs.pkgs.haskell.packages."${compiler}";
}
