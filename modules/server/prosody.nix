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
    # Client connections (direct TLS)
    5223
    # Server-to-server connections
    5269
    # Server-to-server connections (direct TLS)
    5270
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
      modules.server_contact_info = true;
      extraModules = [ "http_openmetrics" ];
      extraConfig = ''
        storage = "sql";
        sql = {
          driver = "PostgreSQL";
          database = "prosody";
          username = "prosody";
        }
        c2s_direct_tls_ports = { 5223 };
        s2s_direct_tls_ports = { 5270 };
        contact_info = {
          abuse = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
          admin = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
          security = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
        }
        statistics = "internal";
        statistics_interval = "manual";
      '';
    };
  };

  security.acme.certs."kropki.org".extraDomainNames = [
    "conference.kropki.org"
    "upload.kropki.org"
  ];

  users.groups.acme.members = [
    "prosody"
  ];
}
