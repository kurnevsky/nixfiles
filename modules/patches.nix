{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/c531d48ec60b2f0d6c76a6b991f5680525f9fff2.diff";
        sha256 = "sha256-zpIJtr8ZYCcoRzHDBmi8gOPZqhekJziam1MOSD8HB/o=";
      })
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/5726bce7154ee34d3e1faf55612ddde6ccf4faff.diff";
        sha256 = "sha256-uGJDWFXmSZYE9uEqbHJrlSlU0QdU6pkcm8zG+t+V8eE=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [ (self: super: { iosevka = patchedPkgs.iosevka; }) ];
}
