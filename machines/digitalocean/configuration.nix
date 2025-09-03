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
      ensureDatabases = [
        "tt_rss"
      ];
      ensureUsers = [
        {
          name = "tt_rss";
          ensureDBOwnership = true;
        }
      ];
    };
    tt-rss = {
      enable = true;
      virtualHost = null;
      sessionCookieLifetime = 2592000;
      selfUrlPath = "https://kurnevsky.net/tt-rss/";
    };
    phpfpm.pools.tt-rss.settings = {
      "pm.max_children" = 10;
      "pm.start_servers" = 2;
      "pm.min_spare_servers" = 1;
      "pm.max_spare_servers" = 2;
      "pm.max_requests" = 100;
    };
    nginx = {
      enable = true;
      package = pkgs.nginxQuic;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      proxyTimeout = "300s";
      appendHttpConfig = ''
        fastcgi_param HTTP_HOST $host;
      '';
      virtualHosts = {
        "kurnevsky.net" =
          let
            index = pkgs.writeTextDir "index.html" (builtins.readFile ./index.html);
            robots = pkgs.writeTextDir "robots.txt" ''
              User-agent: *
              Disallow: /
            '';
            root = pkgs.symlinkJoin {
              name = "root";
              paths = [
                index
                robots
              ];
            };
          in
          {
            http3 = true;
            quic = true;
            enableACME = true;
            forceSSL = true;
            kTLS = true;
            root = "${root}";
            locations = {
              "= /tt-rss".return = "301 $request_uri/";
              "/tt-rss/" = {
                alias = "${config.services.tt-rss.root}/www/";
                index = "index.php";
              };
              "^~ /tt-rss/feed-icons/".alias = "${config.services.tt-rss.root}/feed-icons/";
              "~ /tt-rss/.+\\.php$" = {
                alias = "${config.services.tt-rss.root}/www/";
                extraConfig = ''
                  fastcgi_split_path_info ^/tt-rss/(.+\.php)(.*)$;
                  fastcgi_pass unix:${config.services.phpfpm.pools.${config.services.tt-rss.pool}.socket};
                  fastcgi_index index.php;
                '';
              };
            };
          };
        "kropki.org" = {
          default = true;
          http3 = true;
          quic = true;
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          root = "/kropki";
          locations = {
            "/ws" = {
              proxyPass = "http://localhost:8080";
              proxyWebsockets = true;
            };
            "/wssh" = {
              proxyPass = "http://localhost:58546";
              proxyWebsockets = true;
            };
            "/wswg" = {
              proxyPass = "http://localhost:57411";
              proxyWebsockets = true;
            };
            "/static/" = {
              alias = "/srv/www/";
              tryFiles = "$uri =404";
              extraConfig = "expires 24h;";
            };
          };
        };
      };
    };
  };

  systemd.services.tt-rss.wantedBy = pkgs.lib.mkForce [ ];

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
