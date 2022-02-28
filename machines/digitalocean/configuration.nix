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
      ensureDatabases = [ "ttrss" ];
      ensureUsers = [{
        name = "ttrss";
        ensurePermissions = { "DATABASE \"ttrss\"" = "ALL PRIVILEGES"; };
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
      virtualHosts."kurnevsky.me" = {
        enableACME = true;
        forceSSL = true;
        # TODO: enable after new nixos release
        # kTLS = true;
        root = "${config.services.tt-rss.root}/www";
        locations = {
          "/" = { index = "index.php"; };
          "^~ /feed-icons" = { root = config.services.tt-rss.root; };
          "~ \\.php$" = {
            extraConfig = ''
              fastcgi_split_path_info ^(.+\.php)(/.+)$;
              fastcgi_pass unix:${
                config.services.phpfpm.pools.${config.services.tt-rss.pool}.socket
              };
              fastcgi_index index.php;
              include ${pkgs.nginxMainline}/conf/fastcgi_params;
              include ${pkgs.nginxMainline}/conf/fastcgi.conf;
            '';
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
