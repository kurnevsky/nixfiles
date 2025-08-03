{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        device = "/dev/sda";
        content = {
          type = "gpt";
          partitions = {
            esp = {
              size = "512M";
              type = "ef00";
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
