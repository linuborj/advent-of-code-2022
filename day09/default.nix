{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { common = ../common; };
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (
      with pkgs.haskellPackages;
      [ cabal-install
        ghcid
      ]
    );
}
