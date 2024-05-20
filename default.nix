let
  pkgs = import <nixpkgs> { };
  myHaskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      servant-auth-server = pkgs.haskell.lib.dontCheck (self.callHackage "servant-auth-server" "0.4.8.0" { });
    };
  };
in
myHaskellPackages.developPackage {
  root = ./.;
}
