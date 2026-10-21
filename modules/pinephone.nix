{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:

{
  imports = [ ./pinephone-plasma-mobile ];

  boot.tmp.cleanOnBoot = true;

  nixpkgs.overlays = [
    (_self: super: {
      # The front camera modes set the ISP source pad without SkipTry. The TRY
      # is answered with the default 800x600 crop, so libmegapixels skips the
      # ACTIVE set and STREAMON fails with EPIPE on isp0.
      libmegapixels = super.libmegapixels.overrideAttrs (old: {
        postPatch = (old.postPatch or "") + ''
          substituteInPlace "config/pine64,pinephone-pro.conf" \
            --replace-fail \
              '{Type: "Mode", Entity: "rkisp1_isp", Pad: 2, Format: "BGGR8"},' \
              '{Type: "Mode", Entity: "rkisp1_isp", Pad: 2, Format: "BGGR8", SkipTry: true},' \
            --replace-fail \
              '{Type: "Mode", Entity: "rkisp1_isp", Pad: 2, Format: "BGGR8", Width: 1048, Height: 780},' \
              '{Type: "Mode", Entity: "rkisp1_isp", Pad: 2, Format: "BGGR8", Width: 1048, Height: 780, SkipTry: true},'
        '';
      });
    })
  ];

  environment.systemPackages = with pkgs; [
    gnupg
    firefox-mobile
    telegram-desktop
    wesnoth
    megapixels
    android-tools
    usbutils
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
      wifi.powersave = true;
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
