{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "82.196.15.1";
    defaultGateway6 = "2a03:b0c0:0:1010::1";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "82.196.15.215";
            prefixLength = 24;
          }
          {
            address = "10.14.0.5";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "2a03:b0c0:0:1010::4c:5001";
            prefixLength = 64;
          }
          {
            address = "fe80::4c13:81ff:fe05:22f8";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [{
          address = "82.196.15.1";
          prefixLength = 32;
        }];
        ipv6.routes = [{
          address = "2a03:b0c0:0:1010::1";
          prefixLength = 128;
        }];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="4e:13:81:05:22:f8", NAME="eth0"
    ATTR{address}=="aa:4a:32:9c:cd:b0", NAME="eth1"
  '';
}
