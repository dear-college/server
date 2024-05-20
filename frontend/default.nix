{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage rec {
  pname = "frontend";
  version = "0.1.0";

  src = pkgs.lib.cleanSource ./.;

  npmDepsHash = "sha256-MQxW0zKgUKR6djJ1kaDvtmq1qSYJYY3X0safakRjs1A=";

  # The prepack script runs the build script, which we'd rather do in the build phase.
  npmPackFlags = [ "--ignore-scripts" ];

  NODE_OPTIONS = "--openssl-legacy-provider";

  meta = {
    description = "frontend code for dear.college";
    homepage = "https://dear.college/";
  };
}
