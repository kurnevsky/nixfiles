{ pkgs, lib, ... }:

{
  users.users.kurnevsky.extraGroups =
    [ "dialout" "networkmanager" "video" "pipewire" ];

  mobile = {
    beautification.splash = true;
    # Setup USB gadget networking in initrd.
    boot.stage-1.networking.enable = lib.mkDefault true;
  };

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager = {
      enable = true;
      # Ensures any rndis config from stage-1 is not clobbered by NetworkManager.
      unmanaged = [ "rndis0" "usb0" ];
    };
  };

  hardware = {
    bluetooth.enable = true;
    # Enabled in plasma mobile by default.
    pulseaudio.enable = lib.mkForce false;
    sensor.iio.enable = true;
  };

  services = {
    openssh.enable = true;
    pipewire = {
      enable = true;
      systemWide = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
    xserver = {
      enable = true;
      desktopManager.plasma5.mobile.enable = true;
      displayManager = {
        lightdm = {
          enable = true;
          extraSeatDefaults = ''
            session-cleanup-script=${pkgs.procps}/bin/pkill -P1 -fx ${pkgs.lightdm}/sbin/lightdm
          '';
        };
        defaultSession = "plasma-mobile";
        autoLogin = {
          enable = true;
          user = "kurnevsky";
        };
      };
    };
  };
}
