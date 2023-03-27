{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/kurnevsky/nixpkgs/commit/98ef4e4fc9d0638c17a5294ae2a4b1d2d370125d.diff";
        sha256 = "sha256-EHeMnoH+tZnpdIycQYLBdu7Vvef4cAdDxAzTnUN5tRQ=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [
    (self: super: {
      tor-browser-bundle-bin = patchedPkgs.tor-browser-bundle-bin;
    })
  ];
}
