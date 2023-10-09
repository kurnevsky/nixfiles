{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/df9c6eb7815f69edb4f69c48c97d9e848318b626.diff";
        sha256 = "sha256-B8i5dK3RMRGnWiLoq9ujEKWIStaNHXYTkoDvwlZbMjw=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [
    (self: super: {
      tree-sitter = patchedPkgs.tree-sitter;
    })
  ];
}
