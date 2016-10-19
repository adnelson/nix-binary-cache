{
  pkgs ? import <nixpkgs> {config.allowUnfree = true;},
  compiler ? "ghc7102"
}:

let
  haskellPackages = pkgs.haskell.packages."${compiler}";
in

import ./project.nix {
  inherit pkgs haskellPackages;
}
