{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:

{
  nixpkgs.overlays = [
    (final: super: {
      mobile-nixos = super.mobile-nixos // {
        # Fix the cut-down stage-1 packages from the mobile-nixos boot overlay:
        # - libinput: lua is stripped from buildInputs while the meson flags
        #   still enable lua plugins;
        # - libxkbcommon: tools fail to compile without xkeyboard-config, and
        #   stage-1 only needs the library.
        stage-1 =
          (final.appendOverlays [
            (_final: prev: { libinput = prev.libinput.override { luaSupport = false; }; })
            (import "${inputs.mobile-nixos}/boot/overlay")
            (_final: prev: {
              libxkbcommon = prev.libxkbcommon.overrideAttrs (old: {
                mesonFlags = old.mesonFlags ++ [ "-Denable-tools=false" ];
              });
            })
          ]).mobile-nixos.stage-1;
      };
    })
  ];

  # Mobile-nixos still expects the platform description that was removed from
  # nixpkgs elaborated systems.
  nixpkgs.hostPlatform =
    if config.mobile.system.system == "aarch64-linux" then
      {
        system = "aarch64-linux";
        linux-kernel = {
          name = "aarch64-multiplatform";
          baseConfig = "defconfig";
          DTB = true;
          autoModules = true;
          preferBuiltin = true;
          target = "Image";
        };
      }
    else
      {
        system = config.mobile.system.system;
        linux-kernel = {
          name = "pc";
          baseConfig = "defconfig";
          autoModules = true;
          target = "bzImage";
        };
      };

  boot.tmp.cleanOnBoot = true;

  environment.systemPackages = with pkgs; [
    gnupg
    firefox-mobile
    telegram-desktop
    wesnoth
    megapixels
    gnome-console
  ];

  i18n.supportedLocales = [
    "C.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "ru_RU.UTF-8/UTF-8"
  ];

  users.users.kurnevsky.extraGroups = [
    "dialout"
    "networkmanager"
    "video"
    "pipewire"
    "audio"
  ];

  mobile = {
    beautification.splash = true;
    # Setup USB gadget networking in initrd.
    boot.stage-1.networking.enable = lib.mkDefault true;
  };

  zramSwap.enable = true;

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager = {
      enable = true;
      # Ensures any rndis config from stage-1 is not clobbered by NetworkManager.
      unmanaged = [
        "rndis0"
        "usb0"
      ];
    };
  };

  security.rtkit.enable = true;

  hardware = {
    bluetooth.enable = true;
    sensor.iio.enable = true;
    # The default accesses `kernel.buildDTBs` which the mobile-nixos kernel
    # builder doesn't define.
    deviceTree.enable = lib.mkDefault pkgs.stdenv.hostPlatform.isAarch64;
  };

  services = {
    displayManager.autoLogin = {
      enable = true;
      user = "kurnevsky";
    };
    xserver = {
      enable = true;
      desktopManager.phosh = {
        enable = true;
        user = "kurnevsky";
        group = "users";
      };
      displayManager.lightdm = {
        enable = true;
        extraSeatDefaults = ''
          session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
        '';
      };
    };
  };

  home-manager.users.kurnevsky = {
    programs.git = {
      enable = true;
      signing.format = "openpgp";
    };
    services = {
      gpg-agent = {
        enable = true;
        pinentry.package = pkgs.pinentry-qt;
      };
    };
  };
}
