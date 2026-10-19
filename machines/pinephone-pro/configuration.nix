{
  boot.kernelParams = [ "brcmfmac.feature_disable=0x200000" ];

  # Root is on LUKS (/dev/dm-0), which the growpart script can't handle,
  # and the installer already sized the partition to fill the disk.
  boot.growPartition = false;

  networking.hostName = "pinephone-pro";

  mobile.boot.stage-1.luks.reusePassphrases = true;

  boot.initrd.luks.devices."LUKS-PINEPHONE-PRO-HOME" = {
    device = "/dev/mmcblk1";
    allowDiscards = true;
  };

  fileSystems."/home" = {
    device = "/dev/mapper/LUKS-PINEPHONE-PRO-HOME";
    fsType = "btrfs";
    options = [
      "compress=zstd"
      "ssd_spread"
      "discard=async"
      "noatime"
      "autodefrag"
      "commit=60"
    ];
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-pinephone-pro.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-pinephone-pro.age;
    hans.file = ../../secrets/hans.age;
    iodine.file = ../../secrets/iodine.age;
    shadowsocks.file = ../../secrets/shadowsocks.age;
  };

  system.stateVersion = "25.11";

  home-manager.users = {
    root.home.stateVersion = "25.11";
    kurnevsky.home.stateVersion = "25.11";
  };
}
