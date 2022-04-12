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

    nur = {
      type = "github";
      owner = "nix-community";
      repo = "nur";
    };
  };

  outputs = { self, ... }@inputs:
    let
      commonModules = [
        ({ pkgs, ... }: {
          _module.args =
            let platform = { inherit (pkgs.stdenv.targetPlatform) system; };
            in { nixpkgs-unstable = import inputs.nixpkgs-unstable platform; };
        })
        inputs.home-manager.nixosModules.home-manager
        ./modules/common.nix
        ./modules/bfq.nix
        ./modules/zswap.nix
        ./modules/patches.nix
        (import ./modules/common-home.nix [ "root" "kurnevsky" ])
      ];
      desktopModules = commonModules ++ [
        (args: {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlay
            inputs.nur.overlay
          ];
        })
        (import ./modules/common-home.nix [ "ww" ])
        ./modules/desktop.nix
        ./modules/wayland.nix
        ./modules/kde.nix
        ./modules/sandbox.nix
        ./modules/shadowsocks.nix
        ./modules/shapeshifter-client.nix
        ./modules/motion.nix
        ./modules/rnnoise.nix
        ./modules/torjail.nix
        ./modules/nspawn.nix
        ./modules/overlays.nix
      ];
    in {
      nixosConfigurations = {
        dell = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/dell/configuration.nix
            ./machines/dell/hardware-configuration.nix
          ];
        };
        evo = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/evo/configuration.nix
            ./machines/evo/hardware-configuration.nix
          ];
        };
        acer = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            (import ./modules/common-home.nix [ "parents" ])
            ./machines/acer/hardware-configuration.nix
            ./machines/acer/configuration.nix
          ];
        };
        digitalocean = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            ./modules/shadowsocks-server.nix
            ./modules/shapeshifter-server.nix
            ./machines/digitalocean/configuration.nix
            ./machines/digitalocean/hardware-configuration.nix
            ./machines/digitalocean/networking.nix
          ];
        };
      };
    };
}
