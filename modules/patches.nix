{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/453281114f968f1c355c14db82555c80b08ed568.diff";
        sha256 = "sha256-2TNCoB6WcaRGxw/HgPK0aWZh8Mt7dXu/cTyZOvFwyIM=";
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
