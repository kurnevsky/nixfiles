{
  lib,
  pkgs,
  config,
  ...
}:

{
  networking.firewall.allowedTCPPorts = [
    # File transfer proxy
    5000
    # Client connections
    5222
    # Server-to-server connections
    5269
    # HTTPS
    5281
  ];

  services = {
    postgresql = {
      ensureDatabases = [
        "prosody"
      ];
      ensureUsers = [
        {
          name = "prosody";
          ensureDBOwnership = true;
        }
      ];
    };

    prosody = {
      enable = true;
      package = pkgs.prosody.override {
        withExtraLuaPackages = lua: [ lua.luadbi-postgresql ];
      };
      admins = [ "kurnevsky@kropki.org" ];
      ssl.cert = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
      ssl.key = "${config.security.acme.certs."kropki.org".directory}/key.pem";
      virtualHosts."kropki.org" = {
        enabled = true;
        domain = "kropki.org";
        ssl.cert = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
        ssl.key = "${config.security.acme.certs."kropki.org".directory}/key.pem";
      };
      muc = [ { domain = "conference.kropki.org"; } ];
      httpFileShare.domain = "upload.kropki.org";
      httpInterfaces = [
        "127.0.0.1"
        "::1"
      ];
      extraModules = [ "http_openmetrics" ];
      extraConfig = ''
        storage = "sql";
        sql = {
          driver = "PostgreSQL";
          database = "prosody";
          username = "prosody";
        }
        statistics = "internal";
        statistics_interval = "manual";
      '';
    };
  };

  users.groups.acme.members = [
    "prosody"
  ];
}
