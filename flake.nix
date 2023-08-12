{
  inputs = {
    nixpkgs = {
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
      owner = "Luflosi";
      repo = "mobile-nixos";
      ref = "eg25-manager";
      flake = false;
    };

    nix-on-droid = {
      type = "github";
      owner = "t184256";
      repo = "nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };
  };

  outputs = inputs:
    let
      users = {
        root = "root";
        kurnevsky = "kurnevsky";
        ww = "ww";
        parents = "parents";
      };
      for-all-home-users = import ./modules/for-all-home-users.nix;
      common-home = import ./modules/common-home.nix;
      commonModules = [
        inputs.home-manager.nixosModules.home-manager
        ./modules/common.nix
        ./modules/bfq.nix
        ./modules/patches.nix
        (for-all-home-users (with users; [ root kurnevsky ]) common-home)
      ];
      desktopModules = commonModules ++ [
        {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            inputs.nur.overlay
          ];
        }
        (for-all-home-users (with users; [ ww ]) common-home)
        (import ./modules/emacs.nix (with users; [ kurnevsky ww ]))
        ./modules/font-freezing.nix
        ./modules/desktop.nix
        ./modules/kde.nix
        ./modules/sandbox.nix
        ./modules/shadowsocks.nix
        ./modules/websocat-ssh.nix
        ./modules/websocat-wg.nix
        ./modules/motion.nix
        ./modules/rnnoise.nix
        ./modules/torjail.nix
        ./modules/torbrowser.nix
        ./modules/nspawn.nix
        ./modules/zswap.nix
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
            (for-all-home-users (with users; [ parents ]) common-home)
            ./modules/zswap.nix
            ./modules/overlays.nix
            ./machines/acer/hardware-configuration.nix
            ./machines/acer/configuration.nix
          ];
        };
        digitalocean = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            ./modules/shadowsocks-server.nix
            ./modules/websocat-ssh-server.nix
            ./modules/websocat-wg-server.nix
            ./modules/zswap.nix
            ./machines/digitalocean/configuration.nix
            ./machines/digitalocean/hardware-configuration.nix
            ./machines/digitalocean/networking.nix
          ];
        };
        pinephone-vm = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = commonModules ++ [
            (import "${inputs.mobile-nixos}/lib/configuration.nix" {
              device = "uefi-x86_64";
            })
            ./modules/pinephone.nix
            ./machines/pinephone-vm/configuration.nix
          ];
        };
        pinephone-vm-encrypted = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            {
              _module.args.rootfs =
                inputs.self.nixosConfigurations.pinephone-vm.config.mobile.outputs.rootfs;
            }
            (import "${inputs.mobile-nixos}/lib/configuration.nix" {
              device = "uefi-x86_64";
            })
            ./modules/pinephone.nix
            ./machines/pinephone-vm-encrypted/configuration.nix
          ];
        };
        pinephone = inputs.nixpkgs.lib.nixosSystem {
          system = "aarch64-linux";
          modules = commonModules ++ [
            (import "${inputs.mobile-nixos}/lib/configuration.nix" {
              device = "pine64-pinephone";
            })
            ./modules/pinephone.nix
            ./machines/pinephone/configuration.nix
            ./machines/pinephone/hardware-configuration.nix
          ];
        };
      };
      nixOnDroidConfigurations.default =
        inputs.nix-on-droid.lib.nixOnDroidConfiguration {
          modules = [
            ./modules/android.nix
            ./machines/android/configuration.nix
            ({ pkgs, ... }: {
              _module.args.emacs-overlay = inputs.emacs-overlay.overlay;
            })
          ];
        };
      packages.x86_64-linux = {
        # nix build -L '/etc/nixos#phone-vm' && ./result -enable-kvm -smp 2
        phone-vm =
          inputs.self.nixosConfigurations.pinephone-vm.config.mobile.outputs.uefi.vm;
        # nix build -L '/etc/nixos#phone-vm-encrypted' && ./result -enable-kvm -smp 2
        phone-vm-encrypted =
          inputs.self.nixosConfigurations.pinephone-vm-encrypted.config.mobile.outputs.uefi.vm;
      };
    };
}
