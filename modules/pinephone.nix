{ pkgs, lib, ... }:

{
  mobile.beautification.splash = true;

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager = {
      enable = true;
      unmanaged = [ "rndis0" "usb0" ];
    };
  };

  hardware = {
    bluetooth.enable = true;
    pulseaudio.enable = lib.mkForce false;
    sensor.iio.enable = true;
  };

  services = {
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
        sddm.enable = true;
        defaultSession = "plasma-mobile";
      };
    };
  };
}
