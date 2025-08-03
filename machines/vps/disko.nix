{
  disko.devices.disk.main = {
    type = "disk";
    device = "/dev/sda";
    content = {
      type = "gpt";
      partitions = {
        esp = {
          size = "512M";
          type = "ef00";
          label = "esp";
          content = {
            type = "filesystem";
            format = "vfat";
            mountpoint = "/boot";
            mountOptions = [ "umask=0077" ];
          };
        };
        root = {
          size = "100%";
          type = "8300";
          label = "root";
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
                "/nix" = {
                  mountpoint = "/nix";
                  mountOptions = options;
                };
              };
          };
        };
      };
    };
  };
}
