{
  disko.devices.disk.main = {
    type = "disk";
    device = "/dev/nvme0";
    content = {
      type = "gpt";
      partitions = {
        esp = {
          size = "512M";
          type = "ef00";
          label = "boot";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        swap = {
          size = "32G";
          type = "8200";
          label = "swap";
          content = {
            type = "swap";
            randomEncryption = true;
            discardPolicy = "both";
          };
        };
        luks = {
          size = "100%";
          type = "8309";
          label = "root";
          content = {
            type = "luks";
            name = "root";
            extraFormatArgs = [
              "--key-size 512"
              "--hash sha512"
            ];
            settings.allowDiscards = true;
            content = {
              type = "btrfs";
              subvolumes =
                let
                  options = [
                    "compress=zstd:3"
                    "noatime"
                    "nodiratime"
                    "ssd"
                    "space_cache=v2"
                  ];
                in
                {
                  "/root" = {
                    mountpoint = "/";
                    mountOptions = options;
                  };
                  "/home" = {
                    mountpoint = "/home";
                    mountOptions = options;
                  };
                };
            };
          };
        };
      };
    };
  };
}
