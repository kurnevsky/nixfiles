{ config, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
      };
    };
    initrd.luks.devices.root.allowDiscards = true;
    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
      v4l2loopback
    ];
    kernelModules = [ "v4l2loopback" "acpi_call" ];
  };

  fileSystems = {
    "/".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/home".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
  };

  swapDevices = [{
    device = "/dev/nvme0n1p2";
    randomEncryption.enable = true;
    # TODO: set discardPolicy after next nixos release
  }];

  networking = {
    hostName = "evo";
    wireguard.interfaces.wg0.ips = [ "192.168.14.3/32" ];
  };

  hardware = {
    cpu.intel.updateMicrocode = true;
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      intel-media-driver
      libvdpau-va-gl
    ];
  };

  services.xserver = {
    videoDrivers = [ "intel" ];
    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  system.stateVersion = "21.05";

  home-manager.users = {
    root.home.stateVersion = "21.05";
    kurnevsky.home.stateVersion = "21.05";
    ww.home.stateVersion = "21.05";
  };
}
