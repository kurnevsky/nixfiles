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
          "https://github.com/NixOS/nixpkgs/commit/61cb7385bd311a6f55813f123c960adab699c96c.diff";
        sha256 = "sha256-8QchRS+IdXxHzJSvUgMMCnDwNVxmxPeyXlj/BvxAVoc=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [
    (self: super: {
      iosevka = patchedPkgs.iosevka;
      wesnoth = patchedPkgs.wesnoth;
    })
  ];
}
