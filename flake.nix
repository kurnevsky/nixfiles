{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    nixpkgs-llvm-15 = {
      type = "github";
      owner = "Et7f3";
      repo = "nixpkgs";
      ref = "add_llvmPackages_15";
    };

    nixpkgs-obs-backgroundremoval = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "04f574a1c0fde90b51bf68198e2297ca4e7cccf4";
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
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      type = "github";
      owner = "nix-community";
      repo = "home-manager";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      type = "github";
      owner = "nix-community";
      repo = "nur";
    };

    mobile-nixos = {
      type = "github";
      owner = "nixos";
      repo = "mobile-nixos";
      flake = false;
    };
  };

  outputs = { self, ... }@inputs:
    let
      users = {
        root = "root";
        kurnevsky = "kurnevsky";
        ww = "ww";
        parents = "parents";
      };
      commonModules = [
        ({ pkgs, ... }: {
          _module.args =
            let platform = { inherit (pkgs.stdenv.targetPlatform) system; };
            in {
              nixpkgs-obs-backgroundremoval =
                import inputs.nixpkgs-obs-backgroundremoval platform;
              nixpkgs-llvm-15 = import inputs.nixpkgs-llvm-15 platform;
            };
        })
        inputs.home-manager.nixosModules.home-manager
        ./modules/common.nix
        ./modules/bfq.nix
        ./modules/zswap.nix
        ./modules/patches.nix
        (import ./modules/common-home.nix (with users; [ root kurnevsky ]))
      ];
      desktopModules = commonModules ++ [
        (args: {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            inputs.nur.overlay
          ];
        })
        (import ./modules/common-home.nix (with users; [ ww ]))
        (import ./modules/emacs.nix (with users; [ kurnevsky ww ]))
        ./modules/desktop.nix
        ./modules/kde.nix
        ./modules/sandbox.nix
        ./modules/shadowsocks.nix
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
        pc = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/pc/configuration.nix
            ./machines/pc/hardware-configuration.nix
          ];
        };
        acer = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            (import ./modules/common-home.nix (with users; [ parents ]))
            ./machines/acer/hardware-configuration.nix
            ./machines/acer/configuration.nix
          ];
        };
        digitalocean = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            ./modules/shadowsocks-server.nix
            ./machines/digitalocean/configuration.nix
            ./machines/digitalocean/hardware-configuration.nix
            ./machines/digitalocean/networking.nix
          ];
        };
        phone-vm = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            { _module.args = { inherit inputs; }; }
            (import "${inputs.mobile-nixos}/lib/configuration.nix" {
              device = "uefi-x86_64";
            })
            ./modules/phone.nix
            ./machines/phone/configuration.nix
          ];
        };
      };
      packages.x86_64-linux.phone-vm =
        inputs.self.nixosConfigurations.phone-vm.config.mobile.outputs.uefi.vm;
    };
}
