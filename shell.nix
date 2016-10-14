{
  pkgs ? import <nixpkgs> {config.allowUnfree = true;},
  compiler ? "ghc7102"
}:
(import ./default.nix { inherit pkgs compiler; }).env
