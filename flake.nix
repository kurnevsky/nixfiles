{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-21.11";
    };

    nixpkgs-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    nixpkgs-master = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };

    nixos-hardware = {
      type = "github";
      owner = "NixOS";
      repo = "nixos-hardware";
      ref = "master";
    };

    fenix = {
      type = "github";
      owner = "nix-community";
      repo = "fenix";
      inputs.nixpkgs.follows = "nixpkgs";
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
      ref = "release-21.11";
    };
  };

  outputs = { self, ... }@inputs:
    let
      commonModules = [
        ({ pkgs, ... }: {
          _module.args =
            let platform = { inherit (pkgs.stdenv.targetPlatform) system; };
            in {
              nixpkgs-unstable = import inputs.nixpkgs-unstable platform;
              nixpkgs-master = import inputs.nixpkgs-master platform;
            };
        })
        inputs.home-manager.nixosModules.home-manager
        (args: {
          nixpkgs.overlays =
            [ inputs.emacs-overlay.overlay inputs.fenix.overlay ];
        })
        ./modules/common.nix
        ./modules/wayland.nix
        ./modules/kde.nix
        ./modules/sandbox.nix
        ./modules/zswap.nix
        ./modules/bfq.nix
        ./modules/shadowsocks.nix
        ./modules/motion.nix
        ./modules/rnnoise.nix
        ./modules/torjail.nix
        ./modules/nspawn.nix
        ./modules/overlays.nix
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
            inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480
            ./modules/validity.nix
            ./machines/evo/configuration.nix
            ./machines/evo/hardware-configuration.nix
          ];
        };
      };
    };
}
