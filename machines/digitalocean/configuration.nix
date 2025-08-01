{
  lib,
  pkgs,
  config,
  ...
}:
{
  boot.tmp.cleanOnBoot = true;

  swapDevices = [
    {
      device = "/swap";
      size = 1024;
    }
  ];

  networking = {
    hostName = "digitalocean";
    nat = {
      enable = true;
      internalInterfaces = [
        "wg0"
        "icmp"
        "dns0"
      ];
    };
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # SMTP
        25
        # HTTP
        80
        # HTTPS
        443
        # SMTPS
        465
        # SMTPS
        587
        # IMAPS
        993
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
        # Yggdrasil
        42853
      ];
      trustedInterfaces = [
        "wg0"
        "icmp"
        "dns0"
      ];
    };
    wireguard.interfaces.wg0 = {
      ips = [ "192.168.14.1/32" ];
      listenPort = 51871;
      privateKeyFile = config.age.secrets.wg-private.path or "/secrets/wg/private.key";
      peers = [
        {
          publicKey = "aRD0dqodCPyqTklk0KinKiTXYTnIBXZ0WFKy/q0dhQo=";
          presharedKeyFile = config.age.secrets.wg-preshared-dell.path or "/secrets/wg/home.psk";
          allowedIPs = [ "192.168.14.2/32" ];
        }
        {
          publicKey = "v69zSw9Ny+ym3DReKRh0gt+Ecc2rcTyKsieqnVZ/PwE=";
          presharedKeyFile = config.age.secrets.wg-preshared-evo.path or "/secrets/wg/work.psk";
          allowedIPs = [ "192.168.14.3/32" ];
        }
        {
          publicKey = "7Do1rDKMm8dZLgChf8pkS57Cg2A/jEj0JhNEfu0YTHM=";
          presharedKeyFile = config.age.secrets.wg-preshared-acer.path or "/secrets/wg/parents.psk";
          allowedIPs = [ "192.168.14.4/32" ];
        }
        {
          publicKey = "il0KQKwE2+clYFXJT/2mLoC3sRudP3B4g/GR45vlP2E=";
          presharedKeyFile = config.age.secrets.wg-preshared-pc.path or "/secrets/wg/pc.psk";
          allowedIPs = [ "192.168.14.5/32" ];
        }
        {
          publicKey = "B2b19WtF3eKODKHKtA3Nb/z5/5+/VLhKe6IOtxzru1U=";
          allowedIPs = [ "192.168.14.7/32" ];
        }
        {
          publicKey = "+CKl9jJnxex0fbWoLH2z1QSfdjXdkYz/1Juc3ps3WSE=";
          allowedIPs = [ "192.168.14.8/32" ];
        }
      ];
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "kurnevsky@gmail.com";
    certs = {
      "kurnevsky.net".group = "acme";
      "kropki.org".group = "acme";
      "rss.kropki.org".group = "acme";
      "stalwart.kropki.org".group = "acme";
    };
  };

  services = {
    do-agent.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql_17;
      ensureDatabases = [
        "tt_rss"
        "stalwart-mail"
        "kropki"
      ];
      ensureUsers = [
        {
          name = "tt_rss";
          ensureDBOwnership = true;
        }
        {
          name = "stalwart-mail";
          ensureDBOwnership = true;
        }
        {
          name = "kropki";
          ensureDBOwnership = true;
        }
      ];
    };
    miniflux = {
      enable = true;
      config = {
        LISTEN_ADDR = "127.0.0.1:34449";
        BASE_URL = "https://rss.kropki.org";
        HTTPS = 1;
        FORCE_REFRESH_INTERVAL = 15;
        POLLING_FREQUENCY = 30;
        POLLING_SCHEDULER = "entry_frequency";
        SCHEDULER_ENTRY_FREQUENCY_FACTOR = 2;
      };
      adminCredentialsFile = config.age.secrets.miniflux.path or "/secrets/miniflux";
    };
    stalwart-mail = {
      enable = true;
      settings = {
        storage.blob = "db";
        store = {
          db = {
            type = "postgresql";
            host = "/var/run/postgresql";
            database = "stalwart-mail";
          };
        };
        lookup.default.hostname = "kropki.org";
        server = {
          hostname = "mail.kropki.org";
          listener = {
            smtp = {
              bind = "[::]:25";
              protocol = "smtp";
            };
            submission = {
              bind = [ "[::]:587" ];
              protocol = "smtp";
            };
            submissions = {
              bind = [ "[::]:465" ];
              protocol = "smtp";
              tls.implicit = true;
            };
            imaptls = {
              bind = [ "[::]:993" ];
              protocol = "imap";
              tls.implicit = true;
            };
            jmap = {
              bind = [ "[::]:30452" ];
              protocol = "http";
            };
          };
        };
        authentication.fallback-admin = {
          user = "admin";
          secret = "%{env:ADMIN_SECRET}%";
        };
        auth.dkim.sign = [
          {
            "if" = "listener != 'smtp'";
            "then" = "['ed25519']";
          }
          { "else" = false; }
        ];
        signature.ed25519 = {
          private-key = "%{env:DKIM_KEY}%";
          domain = "kropki.org";
          selector = "default";
          headers = [
            "From"
            "Subject"
            "Date"
            "Message-ID"
            "To"
            "Cc"
            "MIME-Version"
            "Content-Type"
            "Content-Transfer-Encoding"
            "Content-ID"
            "Content-Description"
            "Resent-Date"
            "Resent-From"
            "Resent-Sender"
            "Resent-To"
            "Resent-Cc"
            "Resent-Message-ID"
            "In-Reply-To"
            "References"
            "List-Id"
            "List-Help"
            "List-Unsubscribe"
            "List-Subscribe"
            "List-Post"
            "List-Owner"
            "List-Archive"
          ];
          algorithm = "ed25519-sha256";
          canonicalization = "relaxed/relaxed";
        };
        certificate.default = {
          cert = "%{file:${config.security.acme.certs."kropki.org".directory}/fullchain.pem}%";
          private-key = "%{file:${config.security.acme.certs."kropki.org".directory}/key.pem}%";
          default = true;
        };
        email.encryption.append = true;
      };
    };
    matrix-continuwuity = {
      enable = true;
      settings.global = {
        server_name = "kropki.org";
        allow_registration = true;
        registration_token_file = config.age.secrets.continuwuity.path or "/secrets/continuwuity";
      };
    };
    heisenbridge = {
      enable = false;
      homeserver = "http://localhost:6167";
      owner = "@kurnevsky:kropki.org";
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
        "rss.kropki.org" = {
          http3 = true;
          quic = true;
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          locations = {
            "/".proxyPass = "http://localhost:34449";
            "/reactflux/" = {
              alias = "${pkgs.callPackage ./reactflux.nix { baseurl = "/reactflux"; }}/";
              index = "index.html";
              tryFiles = "$uri uri/ /index.html =404";
              extraConfig = "expires 24h;";
            };
            "= /reactflux".return = "301 $request_uri/";
          };
        };
        "stalwart.kropki.org" = {
          http3 = true;
          quic = true;
          enableACME = true;
          forceSSL = true;
          kTLS = true;
          locations."/".proxyPass = "http://localhost:30452";
        };
        matrix-federation = {
          serverName = "kropki.org";
          http3 = true;
          quic = true;
          onlySSL = true;
          sslCertificate = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
          sslCertificateKey = "${config.security.acme.certs."kropki.org".directory}/key.pem";
          kTLS = true;
          listen = lib.cartesianProduct {
            addr = [
              "0.0.0.0"
              "[::0]"
            ];
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
      passwordFile = config.age.secrets.hans.path or "/secrets/hans";
    };
    iodine.server = {
      enable = true;
      ip = "172.18.42.1/24";
      domain = "i.kropki.org";
      extraConfig = "-n 82.196.15.215";
      passwordFile = config.age.secrets.iodine.path or "/secrets/iodine";
    };
    yggdrasil = {
      enable = true;
      settings = {
        Peers = [
          "tls://45.147.198.155:6010"
          "tls://ygg.mkg20001.io:443"
          "quic://vpn.itrus.su:7993"
        ];
        Listen = [
          "tls://0.0.0.0:42853"
          "quic://0.0.0.0:42853"
        ];
      };
      persistentKeys = true;
    };
    tox-node = {
      enable = true;
      keysFile = config.age.secrets.tox.path or "/secrets/tox";
      tcpAddresses = [ ];
      lanDiscovery = false;
      motd = "Hi from tox-rs!";
    };
  };

  systemd.services = {
    stalwart-mail.serviceConfig = {
      RestrictAddressFamilies = [ "AF_UNIX" ];
      EnvironmentFile = "${config.age.secrets.stalwart.path or "/secrets/stalwart"}";
    };
    tox-node.serviceConfig.SupplementaryGroups = "secrets-tox";
    kropki = {
      description = "Kropki server";
      after = [ "network.target" ];
      wants = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        User = "kropki";
        Group = "kropki";
        PrivateTmp = true;
        ProtectSystem = "strict";
        Environment = [ "POSTGRES_SOCKET=/var/run/postgresql" ];
        EnvironmentFile = "${config.age.secrets.kropki.path or "/secrets/kropki"}";
        ExecStart = "${pkgs.callPackage ./kropki-server.nix { }}/bin/kropki";
      };
    };
    tt-rss.wantedBy = pkgs.lib.mkForce [ ];
  };

  users = {
    users = {
      hans = {
        group = "hans";
        isSystemUser = true;
      };
      kropki = {
        group = "kropki";
        isSystemUser = true;
      };
      kurnevsky.linger = true;
    };
    groups = {
      acme.members = [
        "nginx"
        "stalwart-mail"
      ];
      hans = { };
      secrets-tox = { };
      kropki = { };
    };
  };

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-digitalocean.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-digitalocean.age;
    hans = {
      file = ../../secrets/hans.age;
      owner = "hans";
      group = "hans";
    };
    iodine = {
      file = ../../secrets/iodine.age;
      owner = "iodined";
      group = "iodined";
    };
    shadowsocks = {
      file = ../../secrets/shadowsocks.age;
      mode = "440";
      group = "secrets-shadowsocks";
    };
    tox = {
      file = ../../secrets/tox.age;
      mode = "440";
      group = "secrets-tox";
    };
    continuwuity = {
      file = ../../secrets/continuwuity.age;
      owner = "continuwuity";
      group = "continuwuity";
    };
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
    miniflux.file = ../../secrets/miniflux.age;
    stalwart.file = ../../secrets/stalwart.age;
    kropki.file = ../../secrets/kropki.age;
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    kurnevsky = {
      home.stateVersion = "21.11";
      services.syncthing.settings = {
        folders."/home/kurnevsky/Sync".versioning = {
          type = "simple";
          cleanupIntervalS = 86400;
          params = {
            params.cleanoutDays = "32";
            keep = "64";
          };
        };
        options = {
          localAnnounceEnabled = pkgs.lib.mkForce false;
          localAnnouncePort = pkgs.lib.mkForce null;
        };
      };
    };
  };
}
