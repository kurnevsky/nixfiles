{ config, pkgs, ... }:

{
  boot = {
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
    initrd = {
      kernelModules = [ "amdgpu" ];
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
    tmp.tmpfsSize = "87%";
    binfmt.emulatedSystems = [ "aarch64-linux" ];
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
    "/home/kurnevsky/data".options = [
      "noatime"
      "nodiratime"
      "compress=zstd:3"
      "nofail"
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

  environment.systemPackages = with pkgs; [
    radeontop
    (import ../../modules/with-native-optimizations.nix config.networking.hostName (
      pkgs.callPackage ../../modules/stable-diffusion-cpp.nix {
        useRocm = true;
        gpuTargets = "gfx1100";
      }
    ))
    # TODO: broken
    # (
    #   (python3.withPackages (
    #     pkgs: with pkgs; [
    #       # TODO: broken
    #       # torchWithRocm
    #       torch
    #       transformers
    #       sentencepiece
    #       sacremoses
    #       torchvision
    #       diffusers
    #       accelerate
    #       peft
    #       (callPackage ./compel.nix { })
    #     ]
    #   )).override
    #   { ignoreCollisions = true; }
    # )
  ];

  networking.hostName = "pc";

  systemd.network.networks."99-wg0".address = [ "192.168.14.5/32" ];

  hardware = {
    cpu.intel.updateMicrocode = true;
    graphics.extraPackages = with pkgs; [
      vaapiIntel
      intel-media-driver
      libvdpau-va-gl
      intel-compute-runtime
      rocmPackages.clr.icd
      rocmPackages.clr
      mesa.opencl
    ];
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [
        "/"
        "/home/kurnevsky/data"
      ];
    };
    xserver = {
      videoDrivers = [
        "intel"
        "amdgpu"
      ];
      deviceSection = ''
        Option "TearFree" "true"
      '';
    };
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-pc.age;
    ww.file = ../../secrets/ww-pc.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-pc.age;
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
      file = ../../secrets/wg-private-pc.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
    wg-preshared = {
      file = ../../secrets/wg-preshared-pc.age;
      owner = "systemd-network";
      group = "systemd-network";
    };
  };

  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
    ww.home.stateVersion = "22.11";
  };
}
