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
      kernelModules = [ "i915" ];
      luks.devices.root.allowDiscards = true;
    };
    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
      v4l2loopback
    ];
    kernelModules = [
      "v4l2loopback"
      "acpi_call"
    ];
    tmp.tmpfsSize = "75%";
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
      device = "/dev/nvme0n1p2";
      randomEncryption = {
        enable = true;
        allowDiscards = true;
      };
      discardPolicy = "both";
    }
  ];

  networking.hostName = "evo";

  systemd.network.networks."99-wg0".address = [ "192.168.14.3/32" ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    graphics.extraPackages = with pkgs; [
      vaapiIntel
      intel-media-driver
      libvdpau-va-gl
      intel-compute-runtime
      mesa.opencl
    ];
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" ];
    };
    throttled.enable = true;
    fprintd.enable = true;
    xserver = {
      videoDrivers = [ "intel" ];
      deviceSection = ''
        Option "TearFree" "true"
      '';
    };
  };

  security.pam.services = {
    sshd.fprintAuth = false;
    # KDE and SDDM don't support it properly at the moment (sddm imports login).
    login.fprintAuth = false;
    kde.fprintAuth = false;
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-evo.age;
    ww.file = ../../secrets/ww-evo.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-evo.age;
    motion = {
      file = ../../secrets/motion.age;
      mode = "440";
      group = "secrets-motion";
    };
    hans = {
      file = ../../secrets/hans.age;
      owner = "hans";
      group = "hans";
    };
    iodine = {
      file = ../../secrets/iodine.age;
      owner = "iodined";
      group = "iodined";
    };
    shadowsocks = {
      file = ../../secrets/shadowsocks.age;
      mode = "440";
      group = "secrets-shadowsocks";
    };
    wg-private = {
      file = ../../secrets/wg-private-evo.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
    wg-preshared = {
      file = ../../secrets/wg-preshared-evo.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    kurnevsky.home.stateVersion = "21.11";
    ww.home.stateVersion = "21.11";
  };
}
