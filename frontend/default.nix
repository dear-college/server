{ pkgs ? import <nixpkgs> {} }:

pkgs.buildNpmPackage rec {
  pname = "frontend";
  version = "0.1.0";

  src = pkgs.lib.cleanSource ./.;

  npmDepsHash = "sha256-Pq2kt7PXDUDPD0fUA/RcCqZrHGRsLl53tqHg22ZpD4M=";

  # The prepack script runs the build script, which we'd rather do in the build phase.
  npmPackFlags = [ "--ignore-scripts" ];

  NODE_OPTIONS = "--openssl-legacy-provider";

  meta = {
    description = "frontend code for dear.college";
    homepage = "https://dear.college/";
  };
}
