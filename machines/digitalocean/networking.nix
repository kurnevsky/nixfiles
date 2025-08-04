{ lib, ... }:
{
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "1.1.1.1"
    ];
    defaultGateway = "134.122.80.1";
    defaultGateway6 = {
      address = "2a03:b0c0:3:f0::1";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "134.122.80.232";
            prefixLength = 20;
          }
          {
            address = "10.19.0.6";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "2a03:b0c0:3:f0:0:1:232d:2000";
            prefixLength = 64;
          }
          {
            address = "fe80::4c21:bcff:fea8:9d48";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [
          {
            address = "134.122.80.1";
            prefixLength = 32;
          }
        ];
        ipv6.routes = [
          {
            address = "2a03:b0c0:3:f0::1";
            prefixLength = 128;
          }
        ];
      };
      eth1 = {
        ipv4.addresses = [
          {
            address = "10.114.0.3";
            prefixLength = 20;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::88b0:f6ff:fef2:6e60";
            prefixLength = 64;
          }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="4e:21:bc:a8:9d:48", NAME="eth0"
    ATTR{address}=="8a:b0:f6:f2:6e:60", NAME="eth1"
  '';
}
