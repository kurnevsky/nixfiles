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
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [ (self: super: { iosevka = patchedPkgs.iosevka; }) ];
}
