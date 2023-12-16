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
    ((python3.withPackages (pkgs:
      with pkgs; [
        torchWithRocm
        transformers
        sentencepiece
        sacremoses
        torchvision
        diffusers
        accelerate
        peft
        (callPackage ./compel.nix { })
      ])).override { ignoreCollisions = true; })
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
      rocmPackages.clr.icd
      rocmPackages.clr
      mesa.opencl
    ];
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" "/home/kurnevsky/data" ];
    };
    xserver = {
      videoDrivers = [ "intel" "amdgpu" ];
      deviceSection = ''
        Option "TearFree" "true"
      '';
    };
  };

  security.pam.services = {
    sshd.fprintAuth = false;
    # KDE and SDDM don't support it properly at the moment.
    sddm.fprintAuth = false;
    kde.fprintAuth = false;
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-pc.age;
    ww.file = ../../secrets/ww-pc.age;
    store.file = ../../secrets/store-pc.age;
    motion = {
      file = ../../secrets/motion.age;
      mode = "440";
      group = "secrets";
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
  };

  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
    ww.home.stateVersion = "22.11";
  };
}
