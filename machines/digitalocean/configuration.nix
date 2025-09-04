{
  lib,
  pkgs,
  config,
  ...
}:

{
  swapDevices = [
    {
      device = "/swap";
      size = 1024;
    }
  ];

  networking.hostName = "digitalocean";

  services.do-agent.enable = true;

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-digitalocean.age;
    store.file = ../../secrets/store-digitalocean.age;
    syncthing-key = {
      file = ../../secrets/syncthing-key-digitalocean.age;
      owner = "kurnevsky";
      group = "users";
    };
    syncthing-cert = {
      file = ../../secrets/syncthing-cert-digitalocean.age;
      owner = "kurnevsky";
      group = "users";
    };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    kurnevsky.home.stateVersion = "21.11";
  };
}
