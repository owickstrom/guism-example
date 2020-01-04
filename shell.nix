{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:
let
  fontsConf = pkgs.makeFontsConf {
    fontDirectories = [ pkgs.cantarell-fonts ];
  };
  haskellPackages = pkgs.haskell.packages.${compiler};
  project = import ./. { inherit compiler; };
in
  haskellPackages.shellFor {
    withHoogle = true;
    packages = p: [project];
    buildInputs = [];
    FONTCONFIG_FILE = fontsConf;
  }
