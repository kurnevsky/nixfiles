{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-21.05";
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };

    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "release-21.05";
    };
  };

  outputs = { self, ... }@inputs: {
    nixosConfigurations.dell = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        inputs.home-manager.nixosModules.home-manager
        (args: { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; })
        ./modules/common.nix
        ./modules/sandbox.nix
        ./modules/zswap.nix
        ./modules/bfq.nix
        ./modules/shadowsocks.nix
        ./modules/patches.nix
        ./machines/dell/configuration.nix
        ./machines/dell/hardware-configuration.nix
      ];
    };
  };
}
