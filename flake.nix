{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-21.05";
    };

    nixpkgs-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
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

  outputs = { self, ... }@inputs:
    let
      commonModules = [
        ({ pkgs, ... }: {
          _module.args.nixpkgs-unstable = import inputs.nixpkgs-unstable {
            inherit (pkgs.stdenv.targetPlatform) system;
          };
        })
        inputs.home-manager.nixosModules.home-manager
        (args: { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; })
        ./modules/common.nix
        ./modules/xmonad.nix
        ./modules/sandbox.nix
        ./modules/zswap.nix
        ./modules/bfq.nix
        ./modules/shadowsocks.nix
        ./modules/motion.nix
        ./modules/torjail.nix
        ./modules/rnnoise.nix
        ./modules/patches.nix
      ];
    in {
      nixosConfigurations = {
        dell = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            ./machines/dell/configuration.nix
            ./machines/dell/hardware-configuration.nix
          ];
        };
        evo = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            ./modules/validity.nix
            ./machines/evo/configuration.nix
            ./machines/evo/hardware-configuration.nix
          ];
        };
      };
    };
}
