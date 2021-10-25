{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/140966.diff";
        sha256 = "sha256-Ydqvbn4q+oakVRPrQkavXRJMuIGLluO38nNTCTgumqM=";
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
