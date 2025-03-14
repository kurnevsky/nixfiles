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
    kernelParams = [
      "radeon.cik_support=0"
      "radeon.si_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.si_support=1"
    ];
    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
      v4l2loopback
    ];
    kernelModules = [
      "v4l2loopback"
      "acpi_call"
    ];
  };

  fileSystems = {
    "/".options = [
      "noatime"
      "nodiratime"
      "compress=zstd:3"
    ];
    "/home".options = [
      "noatime"
      "nodiratime"
      "compress=zstd:3"
    ];
  };

  swapDevices = [
    {
      device = "/dev/sda2";
      randomEncryption = {
        enable = true;
        allowDiscards = true;
      };
      discardPolicy = "both";
    }
  ];

  networking.hostName = "dell";

  systemd.network.networks."99-wg0".address = [ "192.168.14.2/32" ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    graphics.extraPackages = with pkgs; [
      # Discrete GPU can be used with DRI_PRIME=1 LIBVA_DRIVER_NAME=radeonsi
      vaapiIntel
      libvdpau-va-gl
      mesa.opencl
    ];
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" ];
    };
    udev.extraRules = ''
      ACTION=="add|change", SUBSYSTEM=="usb", TEST=="power/control", ATTR{idVendor}=="0cf3", ATTR{idProduct}=="0036", ATTR{power/control}="on"
    '';
    xserver = {
      videoDrivers = [
        "intel"
        "amdgpu"
      ];
      deviceSection = ''
        Option "TearFree" "true"
      '';
      displayManager.sessionCommands =
        let
          layout = pkgs.writeText "xkb-layout" ''
            ! Bind right super key as menu.
            keycode 134 = Menu
          '';
        in
        "${pkgs.xorg.xmodmap}/bin/xmodmap ${layout}";
    };
  };

  system.stateVersion = "21.05";

  home-manager.users = {
    root.home.stateVersion = "21.05";
    kurnevsky.home.stateVersion = "21.05";
    ww.home.stateVersion = "21.05";
  };
}
