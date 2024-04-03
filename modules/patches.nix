{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
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
  nixpkgs.overlays = [ (_self: _super: { iosevka = patchedPkgs.iosevka; }) ];
}
