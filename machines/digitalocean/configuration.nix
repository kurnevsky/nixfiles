{ pkgs, ... }: {
  boot.cleanTmpDir = true;

  networking.hostName = "digitalocean";

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
      database = {
        name = "ttrss";
        user = "ttrss";
      };
      sessionCookieLifetime = 2592000;
      selfUrlPath = "https://kurnevsky.net/tt-rss/";
    };
    nginx = {
      enable = true;
      package = pkgs.nginxMainline;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts."kurnevsky.net" = {
        enableACME = true;
        forceSSL = true;
      };
    };
  };
}
