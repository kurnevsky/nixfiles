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
        withCommunityModules = [
          "http_altconnect"
          "unified_push"
        ];
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
      # httpFileShare = {
      #   domain = "upload.kropki.org";
      #   http_external_url = "https://upload.kropki.org/";
      # };
      xmppComplianceSuite = false;
      httpInterfaces = [
        "127.0.0.1"
        "::1"
      ];
      httpsInterfaces = [
        "127.0.0.1"
        "::1"
      ];
      modules = {
        bosh = true;
        server_contact_info = true;
      };
      extraModules = [
        "websocket"
        "http_openmetrics"
        "turn_external"
      ];
      extraConfig = ''
        storage = "sql";
        sql = {
          driver = "PostgreSQL";
          database = "prosody";
          username = "prosody";
        }
        trusted_proxies = { "127.0.0.1", "::1" };
        c2s_direct_tls_ports = { 5223 };
        s2s_direct_tls_ports = { 5270 };
        contact_info = {
          abuse = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
          admin = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
          security = { "mailto:kurnevsky@kropki.org", "xmpp:kurnevsky@kropki.org" };
        }
        turn_external_host = "kropki.org";
        turn_external_port = ${builtins.toString config.services.coturn.listening-port};
        turn_external_secret = ENV_TURN_SECRET;
        http_external_url = "https://kropki.org/"
        consider_bosh_secure = true;
        consider_websocket_secure = true;
        statistics = "internal";
        statistics_interval = "manual";

        Component "upload.kropki.org" "http_file_share"
          modules_disabled = { "s2s" }
          http_external_url = "https://upload.kropki.org/"
          http_file_share_daily_quota = 104857600
          http_file_share_expires_after = "1 week"
          http_file_share_size_limit = 10485760
      '';
    };

    nginx.virtualHosts =
      let
        localhost = "http://localhost:5280";
      in
      {
        "kropki.org".locations = {
          "= /xmpp-websocket" = {
            proxyPass = localhost;
            proxyWebsockets = true;
          };
          "= /http-bind".proxyPass = localhost;
          "/push".proxyPass = localhost;
          "= /.well-known/host-meta".proxyPass = localhost;
          "= /.well-known/host-meta.json".proxyPass = localhost;
        };
        "upload.kropki.org" = {
          http3 = true;
          quic = true;
          forceSSL = true;
          kTLS = true;
          sslCertificate = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
          sslCertificateKey = "${config.security.acme.certs."kropki.org".directory}/key.pem";
          locations."/".proxyPass = localhost;
        };
      };
  };

  systemd.services.prosody.serviceConfig.EnvironmentFile =
    config.age.secrets.prosody-turn.path or "/secrets/prosody-turn";

  security.acme.certs."kropki.org".extraDomainNames = [
    "conference.kropki.org"
    "upload.kropki.org"
  ];

  users.groups.acme.members = [
    "prosody"
  ];

  age.secrets.prosody-turn.file = ../../secrets/prosody-turn.age;
}
