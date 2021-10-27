{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/140966.diff";
        sha256 = "sha256-QXnQYoMiy536UjAWuLSC7ivJj5AQVU8rveevYTC091o=";
      })
      (pkgs.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/143127.diff";
        sha256 = "sha256-Q4AWXIrqhSFymVzZ+1EaJRBVAQDCyIHuQVLIzcqNtQE=";
      })
    ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  nixpkgs.overlays = [
    (self: super: {
      tor-browser-bundle-bin = patchedPkgs.tor-browser-bundle-bin;
      pidgin = patchedPkgs.pidgin;
    })
  ];
}
