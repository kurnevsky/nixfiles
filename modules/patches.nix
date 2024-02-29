{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/7a83442851a0c09a07851f4f9cc88fe4d8f73ea5.diff";
        sha256 = "sha256-2lRcs40lkbW9hVaMMNcsagllZirsxxBFhWYhTdiE+A8=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [
    (self: super: {
      wesnoth = patchedPkgs.wesnoth;
    })
  ];
}
