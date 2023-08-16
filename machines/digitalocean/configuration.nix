{ lib, pkgs, config, ... }: {
  boot.tmp.cleanOnBoot = true;

  swapDevices = [{
    device = "/swap";
    size = 1024;
  }];

  networking = {
    hostName = "digitalocean";
    nat = {
      enable = true;
      internalInterfaces = [ "wg0" "icmp" "dns0" ];
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # HTTP
        80
        # HTTPS
        443
        # Matrix federation
        8448
        # Shadowsocks
        29135
        # Tox
        33445
        # Yggdrasil
        42853
      ];
      allowedUDPPorts = [
        # DNS
        53
        # QUIC
        443
        # Shadowsocks
        29135
        # Tox
        33445
        # WireGuard
        51871
      ];
      trustedInterfaces = [ "wg0" "icmp" "dns0" ];
    };
    wireguard.interfaces.wg0 = {
      ips = [ "192.168.14.1/32" ];
      listenPort = 51871;
      privateKeyFile = "/secrets/wg/private.key";
      peers = [
        {
          publicKey = "aRD0dqodCPyqTklk0KinKiTXYTnIBXZ0WFKy/q0dhQo=";
          presharedKeyFile = "/secrets/wg/home.psk";
          allowedIPs = [ "192.168.14.2/32" ];
        }
        {
          publicKey = "v69zSw9Ny+ym3DReKRh0gt+Ecc2rcTyKsieqnVZ/PwE=";
          presharedKeyFile = "/secrets/wg/work.psk";
          allowedIPs = [ "192.168.14.3/32" ];
        }
        {
          publicKey = "7Do1rDKMm8dZLgChf8pkS57Cg2A/jEj0JhNEfu0YTHM=";
          presharedKeyFile = "/secrets/wg/parents.psk";
          allowedIPs = [ "192.168.14.4/32" ];
        }
        {
          publicKey = "il0KQKwE2+clYFXJT/2mLoC3sRudP3B4g/GR45vlP2E=";
          presharedKeyFile = "/secrets/wg/pc.psk";
          allowedIPs = [ "192.168.14.5/32" ];
        }
      ];
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "kurnevsky@gmail.com";
  };

  services = {
    do-agent.enable = true;
    postgresql = {
      enable = true;
      ensureDatabases = [ "tt_rss" ];
      ensureUsers = [{
        name = "tt_rss";
        ensurePermissions = { "DATABASE \"tt_rss\"" = "ALL PRIVILEGES"; };
      }];
    };
    matrix-conduit = {
      enable = true;
      settings.global = {
        database_backend = "rocksdb";
        server_name = "kurnevsky.net";
      };
    };
    mautrix-telegram = {
      enable = false;
      settings = {
        homeserver = {
          address = "http://localhost:6167";
          domain = "kurnevsky.net";
        };
        appservice = rec {
          hostname = "localhost";
          port = 29317;
          address = "http://${hostname}:${toString port}";
        };
        bridge = {
          startup_sync = true;
          sync_direct_chats = true;
          sync_create_limit = 0;
          permissions."@evgeny:kurnevsky.net" = "admin";
          mute_bridging = true;
          tag_only_on_create = false;
        };
      };
      environmentFile = "/secrets/mautrix-telegram";
      serviceDependencies = [ "conduit.service" ];
    };
    heisenbridge = {
      enable = false;
      homeserver = "http://localhost:6167";
      owner = "@evgeny:kurnevsky.net";
      namespaces.users = [
        {
          regex = "@irc_.*";
          exclusive = true;
        }
        {
          regex = "@heisenbridge:.*";
          exclusive = true;
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
        "kurnevsky.net" = let
          index =
            pkgs.writeTextDir "index.html" (builtins.readFile ./index.html);
          robots = pkgs.writeTextDir "robots.txt" ''
            User-agent: *
            Disallow: /
          '';
          root = pkgs.symlinkJoin {
            name = "root";
            paths = [ index robots ];
          };
        in {
          default = true;
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
            "^~ /tt-rss/feed-icons/".alias =
              "${config.services.tt-rss.root}/feed-icons/";
            "~ /tt-rss/.+\\.php$" = {
              alias = "${config.services.tt-rss.root}/www/";
              extraConfig = ''
                fastcgi_split_path_info ^/tt-rss/(.+\.php)(.*)$;
                fastcgi_pass unix:${
                  config.services.phpfpm.pools.${config.services.tt-rss.pool}.socket
                };
                fastcgi_index index.php;
              '';
            };
            "/wssh" = {
              proxyPass = "http://localhost:58546";
              proxyWebsockets = true;
            };
            "/wswg" = {
              proxyPass = "http://localhost:57411";
              proxyWebsockets = true;
            };
            "/_matrix" = {
              proxyPass = "http://localhost:6167";
              proxyWebsockets = true;
            };
            "/static/" = {
              alias = "/srv/www/";
              tryFiles = "$uri =404";
              extraConfig = "expires 24h;";
            };
          };
        };
        matrix-federation = {
          serverName = "kurnevsky.net";
          http3 = true;
          quic = true;
          onlySSL = true;
          sslCertificate = "${
              config.security.acme.certs."kurnevsky.net".directory
            }/fullchain.pem";
          sslCertificateKey =
            "${config.security.acme.certs."kurnevsky.net".directory}/key.pem";
          kTLS = true;
          listen = lib.cartesianProductOfSets {
            addr = [ "0.0.0.0" "[::0]" ];
            port = [ 8448 ];
            ssl = [ true ];
          };
          locations."/_matrix" = {
            proxyPass = "http://localhost:6167";
            proxyWebsockets = true;
          };
        };
      };
    };
    hans.server = {
      enable = true;
      ip = "172.18.43.0";
      extraConfig = "-d icmp -m 1200";
      passwordFile = "/secrets/hans";
    };
    iodine.server = {
      enable = true;
      ip = "172.18.42.1/24";
      domain = "i.kurnevsky.net";
      extraConfig = "-n 82.196.15.215";
      passwordFile = "/secrets/iodine";
    };
    yggdrasil = {
      enable = true;
      settings = {
        Peers = [
          "tls://45.147.198.155:6010"
          "tls://94.103.82.150:8080"
          "tls://ygg-nl.incognet.io:8884"
          "tls://ygg1.ezdomain.ru:11130"
          "tls://ygg.mkg20001.io:443"
        ];
        Listen = [ "tls://0.0.0.0:42853" ];
      };
      persistentKeys = true;
    };
    tox-node = {
      enable = true;
      keysFile = "/secrets/tox";
      tcpAddresses = [ ];
      lanDiscovery = false;
      motd = "Hi from tox-rs!";
    };
  };

  users = {
    users.hans.group = "hans";
    groups.hans = { };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    kurnevsky.home.stateVersion = "21.11";
  };
}
