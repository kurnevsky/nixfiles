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

  outputs =
    inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = inputs.nixpkgs.lib.genAttrs supportedSystems;
      collectFlakeInputs =
        input:
        [ input ] ++ builtins.concatMap collectFlakeInputs (builtins.attrValues (input.inputs or { }));
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
        (for-all-home-users (with users; [
          root
          kurnevsky
        ]) common-home)
        # Keep flake inputs from being garbage collected
        { system.extraDependencies = collectFlakeInputs inputs.self; }
      ];
      desktopModules = commonModules ++ [
        {
          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            inputs.purescript-overlay.overlays.default
            inputs.nur.overlays.default
          ];
        }
        inputs.solaar.nixosModules.default
        (for-all-home-users (with users; [ ww ]) common-home)
        (import ./modules/emacs.nix (
          with users;
          [
            kurnevsky
            ww
          ]
        ))
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
        (
          { pkgs, ... }:
          {
            environment.systemPackages = [ inputs.agenix.packages.${pkgs.system}.default ];
          }
        )
      ];
      llamaOverride =
        pkgs: config: llama:
        import ./modules/with-native-optimizations.nix config.networking.hostName (
          llama.overrideAttrs (old: {
            postInstall =
              (old.postInstall or "")
              + ''
                find $out/bin -type f ! -wholename '*/llama*' -exec ${pkgs.util-linux}/bin/rename "" 'llama-' {} \;
              '';
          })
        );
      llamaDefault =
        { pkgs, config, ... }:
        {
          environment.systemPackages = [
            (llamaOverride pkgs config inputs.llama-cpp.packages.${pkgs.system}.default)
          ];
        };
      llamaRocm =
        gpuTargets:
        { pkgs, config, ... }:
        {
          environment.systemPackages = [
            (llamaOverride pkgs config (
              inputs.llama-cpp.packages.${pkgs.system}.rocm.override {
                rocmGpuTargets = gpuTargets;
              }
            ))
          ];
        };
    in
    {
      formatter = forAllSystems (system: inputs.nixpkgs.legacyPackages.${system}.nixfmt-rfc-style);
      nixosConfigurations = {
        dell = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = desktopModules ++ [
            ./machines/dell/configuration.nix
            ./machines/dell/hardware-configuration.nix
            llamaDefault
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
            (llamaRocm "gfx1100")
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
      nixOnDroidConfigurations.default = inputs.nix-on-droid.lib.nixOnDroidConfiguration {
        pkgs = import inputs.nixpkgs { system = "aarch64-linux"; };
        modules = [
          ./modules/android.nix
          ./machines/android/configuration.nix
          { _module.args.emacs-overlay = inputs.emacs-overlay.overlay; }
        ];
      };
      packages = forAllSystems (_system: {
        # nix build -L '/etc/nixos#phone-vm' && ./result -enable-kvm -smp 2
        phone-vm = inputs.self.nixosConfigurations.pinephone-vm.config.mobile.outputs.uefi.vm;
      });
    };
}
