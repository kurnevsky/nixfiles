{
  lib,
  pkgs,
  config,
  ...
}:

{
  imports = [
    ./pocket-id.nix
    ./stalwart.nix
    ./miniflux.nix
    ./oauth2-proxy.nix
    ./wakapi.nix
    ./prometheus.nix
    ./grafana.nix
    ./scrutiny.nix
    ./wireguard.nix
    ./tox.nix
    ./matrix.nix
    ./yggdrasil.nix
    ./syncthing.nix
    ./hans.nix
    ./iodine.nix
    ./nginx.nix
    ./kropki.nix
    ./tt-rss.nix
  ];

  boot.tmp.cleanOnBoot = true;

  swapDevices = [
    {
      device = "/swap";
      size = 1024;
    }
  ];

  networking = {
    hostName = "digitalocean";
    nat.enable = true;
    firewall.enable = true;
  };

  services = {
    do-agent.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql_17;
    };
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-digitalocean.age;
    github.file = ../../secrets/github.age;
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
