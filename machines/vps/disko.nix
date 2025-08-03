{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02";
              priority = 1;
            };
            ESP = {
              size = "512M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
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
                    "@root" = {
                      mountpoint = "/";
                      mountOptions = options;
                    };
                    "@home" = {
                      mountpoint = "/home";
                      mountOptions = options;
                    };
                    "@nix" = {
                      mountpoint = "/nix";
                      mountOptions = options;
                    };
                  };
              };
            };
          };
        };
      };
    };
  };
}
