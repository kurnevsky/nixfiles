{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/1691576fef01c9d5a7e0ee3eea04fe945f66c6ac.diff";
        sha256 = "sha256-vPYCt8veKmh0eoFMXamtuCW1E1CKSyXGSDn3Ut6Nxcw=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [ (_self: _super: { iosevka = patchedPkgs.iosevka; }) ];
}
