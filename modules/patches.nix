{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/135032.diff";
        sha256 = "sha256-KfBNFOVEhVdIYqYw4QU3CWRuH1ZxahyXlsrq+MOo4tE=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  nixpkgs.overlays = [
    (self: super: {
      tor-browser-bundle-bin = patchedPkgs.tor-browser-bundle-bin;
    })
  ];
}
