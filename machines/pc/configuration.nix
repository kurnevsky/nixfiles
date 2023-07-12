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
    tmp.tmpfsSize = "87%";
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };

  fileSystems = {
    "/".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/home".options = [ "noatime" "nodiratime" "compress=zstd:3" ];
    "/home/kurnevsky/data".options =
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

  environment.systemPackages = with pkgs; [
    radeontop
    wine-ge
    (pkgs.pkgsCross.mingw32.callPackage ./../../modules/vkd3d-proton.nix { })
    (pkgs.pkgsCross.mingwW64.callPackage ./../../modules/vkd3d-proton.nix { })
  ];

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

  nixpkgs.overlays = [
    (self: super: {
      python3 = super.python3.override {
        packageOverrides = python-self: python-super: {
          torchWithRocm =
            python-super.torchWithRocm.override { gpuTargets = [ "gfx1100" ]; };
        };
      };
    })
  ];

  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
    ww.home.stateVersion = "22.11";
  };
}
