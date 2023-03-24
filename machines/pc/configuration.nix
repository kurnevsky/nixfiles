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
    initrd = {
      kernelModules = [ "amdgpu" ];
      luks = {
        reusePassphrases = true;
        devices.root.allowDiscards = true;
      };
    };
    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
      v4l2loopback
    ];
    kernelModules = [ "v4l2loopback" "acpi_call" ];
    tmpOnTmpfsSize = "87%";
  };

  fileSystems = {
    "/".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/home".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/home/kurnevsky/data".options =
      [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/var/lib/monero".options =
      [ "noatime" "nodiratime" "compress=zstd:3" ];
  };

  swapDevices = [{
    device = "/dev/nvme0n1p2";
    randomEncryption = {
      enable = true;
      allowDiscards = true;
    };
    discardPolicy = "both";
  }];

  networking.hostName = "pc";

  systemd.network.networks."99-wg0".address = [ "192.168.14.5/32" ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    opengl.extraPackages = with pkgs; [
      vaapiIntel
      intel-media-driver
      libvdpau-va-gl
      intel-compute-runtime
      rocm-opencl-icd
      rocm-opencl-runtime
    ];
  };

  services.xserver = {
    videoDrivers = [ "intel" "amdgpu" ];
    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  security.pam.services = {
    sshd.fprintAuth = false;
    # KDE and SDDM don't support it properly at the moment.
    sddm.fprintAuth = false;
    kde.fprintAuth = false;
  };

  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
    ww.home.stateVersion = "22.11";
  };
}
