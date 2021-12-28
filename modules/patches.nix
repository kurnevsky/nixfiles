{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [ ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in { nixpkgs.overlays = [ (self: super: { }) ]; }
