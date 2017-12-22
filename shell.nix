{
  pkgs ? import <nixpkgs> {config.allowUnfree = true;},
  compiler ? "ghc802"
}:
(import ./default.nix { inherit pkgs compiler; }).env
