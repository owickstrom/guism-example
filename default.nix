{ pkgs ? import <nixpkgs> {}, compiler ? "ghc865"
, doCheck ? false, doBenchmark ? false
}:

let
  githubHaskellPackage = haskellPkgs: attrs: args:
    let
      prefetched = builtins.fromJSON (builtins.readFile attrs.path);
      src = pkgs.fetchFromGitHub {
        owner = attrs.owner;
        repo = attrs.repo;
        inherit (prefetched) rev sha256;
      };
      packageSrc = if attrs.repoSubDir == "" then src else "${src}/${attrs.repoSubDir}";
    in haskellPkgs.callCabal2nix attrs.repo packageSrc args;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      gi-gtk-declarative = pkgs.haskell.lib.dontCheck (githubHaskellPackage self {
        owner = "owickstrom";
        repo = "gi-gtk-declarative";
        path = nix/gi-gtk-declarative.json;
        repoSubDir = "gi-gtk-declarative";
      } {});
      haskell-gi-overloading = pkgs.haskell.lib.dontHaddock (self.callHackage "haskell-gi-overloading" "1.0" {});
    };
  };
  drv = haskellPackages.callCabal2nix "guism-example" ./. {};
in drv
