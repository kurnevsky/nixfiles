{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/150614.diff";
        sha256 = "sha256-6NICgOIkxPTbToKeIAcTAQVhHe76KEqj8dx7I7G8ulA=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  nixpkgs.overlays =
    [ (self: super: { libtoxcore = patchedPkgs.libtoxcore; }) ];
}
