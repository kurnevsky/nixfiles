{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [ ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules = [ "services/networking/i2pd.nix" ];
  imports = [
    (builtins.fetchurl {
      url =
        "https://raw.githubusercontent.com/NixOS/nixpkgs/ef025e29984d8be59d1295829352d5653042cff6/nixos/modules/services/networking/i2pd.nix";
      sha256 = "1hrqbs1d72hyggpcjcmyzjfh3phfxbppjb0jqg6h9hfq0wk88p6s";
    })
  ];
  nixpkgs.overlays = [ (self: super: { }) ];
}
