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
    initrd.kernelModules = [ "amdgpu" ];
    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
      v4l2loopback
    ];
    kernelModules = [
      "v4l2loopback"
      "acpi_call"
    ];
    tmp.tmpfsSize = "75%";
    binfmt.emulatedSystems = [ "aarch64-linux" ];
  };

  environment.systemPackages = with pkgs; [
    (import ../../modules/with-native-optimizations.nix config.networking.hostName whisper-cpp)
    (import ../../modules/with-native-optimizations.nix config.networking.hostName llama-cpp)
  ];

  networking.hostName = "evo";

  systemd.network.networks."99-wg0".address = [ "192.168.14.3/32" ];

  hardware = {
    cpu.amd.updateMicrocode = true;
    graphics.extraPackages = with pkgs; [
      libvdpau-va-gl
      mesa.opencl
    ];
  };

  services = {
    btrfs.autoScrub = {
      enable = true;
      fileSystems = [ "/" ];
    };
    fprintd.enable = true;
    xserver = {
      videoDrivers = [ "amdgpu" ];
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
    syncthing-key = {
      file = ../../secrets/syncthing-key-evo.age;
      owner = "kurnevsky";
      group = "users";
    };
    syncthing-cert = {
      file = ../../secrets/syncthing-cert-evo.age;
      owner = "kurnevsky";
      group = "users";
    };
  };

  system.stateVersion = "25.05";

  home-manager.users = {
    root.home.stateVersion = "25.05";
    kurnevsky.home.stateVersion = "25.05";
    ww.home.stateVersion = "25.05";
  };
}
