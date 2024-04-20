{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url =
          "https://github.com/NixOS/nixpkgs/commit/709d088cdb74c406bdf0e94f5ed80acd7b7f01f1.diff";
        sha256 = "sha256-Tg2TgaOtBvJsFusv7j7jGrzcmJCifuMMQweghX1r8g4=";
      })
    ];
  };
  patchedPkgs = import patchesDrv { system = "x86_64-linux"; };
in {
  disabledModules = [ ];
  imports = [ ];
  nixpkgs.overlays = [ (_self: _super: { inherit (patchedPkgs) feather; }) ];
}
