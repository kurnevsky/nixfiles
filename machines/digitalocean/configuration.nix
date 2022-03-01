{ pkgs, config, ... }: {
  boot.cleanTmpDir = true;

  networking = {
    hostName = "digitalocean";
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # HTTP
        80
        # HTTPS
        443
        # Tox
        33445
      ];
      allowedUDPPorts = [
        # DNS
        53
        # Tox
        33445
        # WireGuard
        51871
      ];
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
          publicKey = "KTZl5SWnLkMCRAp/vq8McPCDPJqaWK3iMBlk8yME6x4=";
          presharedKeyFile = "/secrets/wg/work.psk";
          allowedIPs = [ "192.168.14.3/32" ];
        }
        {
          publicKey = "7Do1rDKMm8dZLgChf8pkS57Cg2A/jEj0JhNEfu0YTHM=";
          presharedKeyFile = "/secrets/wg/parents.psk";
          allowedIPs = [ "192.168.14.4/32" ];
        }
      ];
    };
  };

  security.acme = {
    acceptTerms = true;
    email = "kurnevsky@gmail.com";
  };

  services = {
    postgresql = {
      enable = true;
      ensureDatabases = [ "tt_rss" ];
      ensureUsers = [{
        name = "tt_rss";
        ensurePermissions = { "DATABASE \"tt_rss\"" = "ALL PRIVILEGES"; };
      }];
    };
    tt-rss = {
      enable = true;
      virtualHost = null;
      sessionCookieLifetime = 2592000;
      selfUrlPath = "https://kurnevsky.me/tt-rss/";
    };
    nginx = {
      enable = true;
      package = pkgs.nginxMainline;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      proxyTimeout = "300s";
      virtualHosts."kurnevsky.me" = let
        index = pkgs.writeTextFile {
          name = "index.html";
          destination = "/index.html";
          text = builtins.readFile ./index.html;
        };
      in {
        enableACME = true;
        forceSSL = true;
        # TODO: enable after new nixos release
        # kTLS = true;
        root = "${index}";
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
          "/ss" = {
            proxyPass = "http://localhost:8388";
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
    hans.server = {
      enable = true;
      ip = "172.18.43.0";
      extraConfig = "-d icmp -m 1200";
      passwordFile = "/secrets/hans";
    };
    iodine.server = {
      enable = true;
      ip = "172.18.42.1/24";
      domain = "i.kurnevsky.me";
      extraConfig = "-n 82.196.15.215";
      passwordFile = "/secrets/iodine";
    };
    tox-node = {
      enable = true;
      keysFile = "/secrets/tox";
      tcpAddresses = [ ];
      lanDiscovery = false;
      motd = "Hi from tox-rs!";
    };
  };

  users.users.hans.group = "nogroup"; # TODO: don't use
}
