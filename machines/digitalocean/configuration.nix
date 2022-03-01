{ pkgs, config, ... }: {
  boot.cleanTmpDir = true;

  networking = {
    hostName = "digitalocean";
    firewall = {
      enable = true;
      allowedTCPPorts = [ 80 443 ];
      allowedUDPPorts = [ 53 ];
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
  };

  users.users.hans.group = "nogroup"; # TODO: don't use
}
