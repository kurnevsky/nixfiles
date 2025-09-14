{
  imports = [ ./cifs.nix ];

  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  networking = {
    hostName = "vps";
    useDHCP = false;
  };

  systemd.network = {
    enable = true;
    networks."10-lan" = {
      matchConfig.Name = "enp1s0";
      networkConfig = {
        DHCP = "ipv4";
        IPv6AcceptRA = true;
        Address = [ "2a01:4f8:c013:71a5::1/64" ];
        Gateway = [ "fe80::1" ];
      };
      linkConfig.RequiredForOnline = "routable";
    };
  };

  services.coturn.listening-ips = [
    "49.12.217.127"
    "2a01:4f8:c013:71a5::1"
  ];

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-vps.age;
    store.file = ../../secrets/store-vps.age;
    syncthing-key = {
      file = ../../secrets/syncthing-key-vps.age;
      owner = "kurnevsky";
      group = "users";
    };
    syncthing-cert = {
      file = ../../secrets/syncthing-cert-vps.age;
      owner = "kurnevsky";
      group = "users";
    };
  };

  system.stateVersion = "25.05";

  home-manager.users = {
    root.home.stateVersion = "25.05";
    kurnevsky.home.stateVersion = "25.05";
  };
}
