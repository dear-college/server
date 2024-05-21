{ pkgs ? import <nixpkgs> {} }:
let
  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      dear-college = self.callCabal2nix "dear-college" ./. {};
      servant-auth-server = pkgs.haskell.lib.dontCheck (self.callHackage "servant-auth-server" "0.4.8.0" { });
    };
  };
in
myHaskellPackages.dear-college

