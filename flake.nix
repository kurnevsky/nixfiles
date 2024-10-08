{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    nixpkgs-old = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "574d1eac1c200690e27b8eb4e24887f8df7ac27c";
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
      owner = "NixOS";
      repo = "mobile-nixos";
      ref = "development";
      flake = false;
    };

    nix-on-droid = {
      type = "github";
      owner = "t184256";
      repo = "nix-on-droid";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    agenix = {
      type = "github";
      owner = "ryantm";
      repo = "agenix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.follows = "home-manager";
    };

    base16 = {
      type = "github";
      owner = "SenchoPens";
      repo = "base16.nix";
    };

    tt-schemes = {
      type = "github";
      owner = "tinted-theming";
      repo = "schemes";
      flake = false;
    };

    solaar = {
      type = "github";
      owner = "Svenum";
      repo = "Solaar-Flake";
      ref = "0.1.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    purescript-overlay = {
      type = "github";
      owner = "thomashoneyman";
      repo = "purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    llama-cpp = {
      type = "github";
      owner = "ggerganov";
      repo = "llama.cpp";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    let
      collectFlakeInputs = input:
        [ input ] ++ builtins.concatMap collectFlakeInputs
        (builtins.attrValues (input.inputs or { }));
      users = {
        root = "root";
        kurnevsky = "kurnevsky";
        ww = "ww";
        parents = "parents";
      };
      for-all-home-users = import ./modules/for-all-home-users.nix;
      common-home = import ./modules/common-home.nix;
      commonModules = [
        inputs.base16.nixosModule
        { scheme = "${inputs.tt-schemes}/base24/one-dark.yaml"; }
        inputs.agenix.nixosModules.default
        ./modules/agenix.nix
        inputs.home-manager.nixosModules.home-manager
        ./modules/common.nix
        ./modules/bfq.nix
        ./modules/overlays.nix
        (for-all-home-users (with users; [ root kurnevsky ]) common-home)
        # Keep flake inputs from being garbage collected
        { system.extraDependencies = collectFlakeInputs inputs.self; }
        ({ pkgs, ... }:
          let
            oldPkgs = import inputs.nixpkgs-old {
              inherit (pkgs.stdenv.targetPlatform) system;
            };
          in {
            nixpkgs.overlays =
              [ (_self: _super: { inherit (oldPkgs) deadbeef; }) ];
          })
        ({ pkgs, ... }: {
          disabledModules = [ "services/networking/i2pd.nix" ];
          imports = [
            (builtins.fetchurl {
              url = "https://raw.githubusercontent.com/NixOS/nixpkgs/1d8136e1ae15484677de11487e74dd2c1fe495d6/nixos/modules/services/networking/i2pd.nix";
              sha256 = "sha256:1mpks4prsj60i5hgxz2kahmjs5j3z028973lffx5z4a4xclqfm5j";
            })
          ];
        })
      ];
      desktopModules = commonModules ++ [
        {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            inputs.purescript-overlay.overlays.default
            inputs.nur.overlay
          ];
        }
        inputs.solaar.nixosModules.default
        (for-all-home-users (with users; [ ww ]) common-home)
        (import ./modules/emacs.nix (with users; [ kurnevsky ww ]))
        ./modules/desktop.nix
        ./modules/kde.nix
        ./modules/sandbox.nix
        ./modules/shadowsocks.nix
        ./modules/websocat-ssh.nix
        ./modules/websocat-wg.nix
        ./modules/motion.nix
        ./modules/torjail.nix
        ./modules/torbrowser.nix
        ./modules/nspawn.nix
        ./modules/zswap.nix
        ({ pkgs, ... }: {
          environment.systemPackages =
            [ inputs.agenix.packages.${pkgs.system}.default ];
        })
      ];
      llamaOverride = pkgs: llama:
        llama.overrideAttrs (old: {
          postInstall = (old.postInstall or "") + ''
            find $out/bin -type f ! -wholename '*/llama*' -exec ${pkgs.util-linux}/bin/rename "" 'llama-' {} \;
          '';
          NIX_CFLAGS_COMPILE = [ "-O3" "-march=native" "-mtune=native" ];
          NIX_ENFORCE_NO_NATIVE = false;
          preferLocalBuild = true;
          allowSubstitutes = false;
        });
      llamaDefault = { pkgs, ... }: {
        environment.systemPackages = [
          (llamaOverride pkgs inputs.llama-cpp.packages.${pkgs.system}.default)
        ];
      };
      llamaOpencl = { pkgs, ... }: {
        environment.systemPackages = [
          (llamaOverride pkgs inputs.llama-cpp.packages.${pkgs.system}.opencl)
        ];
      };
      llamaRocm = { pkgs, ... }: {
        environment.systemPackages = [
          (llamaOverride pkgs inputs.llama-cpp.packages.${pkgs.system}.rocm)
        ];
      };
    in {
      nixosConfigurations = {
        dell = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/dell/configuration.nix
            ./machines/dell/hardware-configuration.nix
            llamaOpencl
          ];
        };
        evo = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/evo/configuration.nix
            ./machines/evo/hardware-configuration.nix
            llamaDefault
          ];
        };
        pc = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/pc/configuration.nix
            ./machines/pc/hardware-configuration.nix
            llamaRocm
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
          pkgs = import inputs.nixpkgs { system = "aarch64-linux"; };
          modules = [
            ./modules/android.nix
            ./machines/android/configuration.nix
            { _module.args.emacs-overlay = inputs.emacs-overlay.overlay; }
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
