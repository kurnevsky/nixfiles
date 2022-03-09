{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [ ];
  };
  patchedPkgs = (import patchesDrv { system = "x86_64-linux"; });
in {
  disabledModules =
    [ "services/networking/i2pd.nix" "services/networking/tox-node.nix" ];
  imports = [
    (builtins.fetchurl {
      url =
        "https://raw.githubusercontent.com/NixOS/nixpkgs/ef025e29984d8be59d1295829352d5653042cff6/nixos/modules/services/networking/i2pd.nix";
      sha256 = "1hrqbs1d72hyggpcjcmyzjfh3phfxbppjb0jqg6h9hfq0wk88p6s";
    })
    (builtins.fetchurl {
      url =
        "https://raw.githubusercontent.com/NixOS/nixpkgs/d94be445264f6e376502fcf07622451423b756d8/nixos/modules/services/networking/tox-node.nix";
      sha256 = "0dn1zqy937sbwkd4qzdpr3mn1l1bvkr9fn2680k3mhidfc2x4k6b";
    })
  ];
  nixpkgs.overlays = [ (self: super: { }) ];
}
