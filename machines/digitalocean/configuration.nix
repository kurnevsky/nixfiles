{ pkgs, ... }: {
  boot.cleanTmpDir = true;

  networking.hostName = "digitalocean";

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
      database = {
        name = "ttrss";
        user = "ttrss";
      };
      sessionCookieLifetime = 2592000;
      selfUrlPath = "https://kurnevsky.net/tt-rss/";
    };
  };
}
