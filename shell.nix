{ nixpkgs ? (import <nixpkgs> {}) }:
let
  lib = nixpkgs.haskell-ng.lib;
  haskell = nixpkgs.haskellngPackages.override {
    overrides = self: super: {
      # overrides here

      machines = lib.overrideCabal super.machines (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "ekmett";
          repo = "machines";
          rev = "c1386ae0cbbd4aa843ad4512a0c19b0c6c8a4786";
          sha256 = "1nlh7b1p7fpxs6dw68h1c77ygspdv30hzi7ryr8v9pjs47f1ala1";
        };
      });
      cabal-helper = lib.overrideCabal super.cabal-helper (drv: {
        src = nixpkgs.fetchFromGitHub {
          owner = "DanielG";
          repo = "cabal-helper";
          rev = "f69f35bc4af49b3a25ed6407375c03f5b7569432";
          sha256 = "0ngim8j159ssxmaqn8vm9znr2gn22jgmc5bwk2z8fgvsxvzrslfl";
        };
        buildDepends = drv.buildDepends ++ [ self.utf8-string self.extra self.unix ];
      });

      ghc-mod = lib.overrideCabal super.ghc-mod (drv: {
        broken = false;
        src = nixpkgs.fetchFromGitHub {
          owner = "kazu-yamamoto";
          repo = "ghc-mod";
          rev = "bfa0b965ee3497f5f41d261072dc6bae0af00a06";
          sha256 = "10id8hmkzw03v7910h7z6m1vzafrwrazjkmnzc993fwrpg0n85i7";
        };
        version = "5.2.1";
        buildDepends = drv.buildDepends ++ [ self.cabal-helper self.cereal ];
      });

      # we override `my-project` to add dev/build dependencies
      # note that because we use `self.callPackage` self will have local-common-lib.
      free-http = lib.addBuildTools (self.callPackage ./free-http.nix {}) [
        # haskell-related build/dev tools 
        self.cabal-install
        self.ghc-mod
        self.hasktags
        self.stylish-haskell
        self.codex

        # non-haskell-related build/dev tools
        self.cabal2nix
      ];
    };
  };
in
haskell.free-http.env
