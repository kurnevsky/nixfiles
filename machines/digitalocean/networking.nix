{ lib, ... }:
{
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "8.8.8.8"
    ];
    defaultGateway = "167.71.48.1";
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
            address = "167.71.63.75";
            prefixLength = 20;
          }
          {
            address = "10.19.0.5";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "2a03:b0c0:3:f0:0:1:2270:f000";
            prefixLength = 64;
          }
          {
            address = "fe80::5c86:feff:fe57:9484";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [
          {
            address = "167.71.48.1";
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
            address = "10.114.0.2";
            prefixLength = 20;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::5803:50ff:fec4:6c60";
            prefixLength = 64;
          }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="5e:86:fe:57:94:84", NAME="eth0"
    ATTR{address}=="5a:03:50:c4:6c:60", NAME="eth1"
  '';
}
