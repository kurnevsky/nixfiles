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
        "https://raw.githubusercontent.com/NixOS/nixpkgs/2f17f2c279718c3d8d7fb045d1f6e1ebc72299bc/nixos/modules/services/networking/tox-node.nix";
      sha256 = "01gsz7gq3486yknjnlrnriv2lbwn8876mjnkvh6d6pnc45zyk16q";
    })
  ];
  nixpkgs.overlays = [ (self: super: { }) ];
}
