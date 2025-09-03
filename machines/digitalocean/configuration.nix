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
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # HTTP
        80
        # HTTPS
        443
      ];
      allowedUDPPorts = [
        # DNS
        53
        # QUIC
        443
      ];
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "kurnevsky@gmail.com";
    certs = {
      "kurnevsky.net".group = "acme";
      "kropki.org".group = "acme";
    };
  };

  services = {
    do-agent.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql_17;
    };
    nginx = {
      enable = true;
      package = pkgs.nginxQuic;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      proxyTimeout = "300s";
      virtualHosts."kropki.org" = {
        default = true;
        http3 = true;
        quic = true;
        enableACME = true;
        forceSSL = true;
        kTLS = true;
        locations."/static/" = {
          alias = "/srv/www/";
          tryFiles = "$uri =404";
          extraConfig = "expires 24h;";
        };
      };
    };
  };

  users.groups.acme.members = [
    "nginx"
  ];

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
