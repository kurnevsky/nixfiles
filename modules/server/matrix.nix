{
  lib,
  config,
  ...
}:

{
  networking.firewall.allowedTCPPorts = [
    # Matrix federation
    8448
  ];

  services = {
    matrix-continuwuity = {
      enable = true;
      settings.global = {
        server_name = "kropki.org";
        allow_registration = true;
        registration_token_file = config.age.secrets.continuwuity.path or "/secrets/continuwuity";
        turn_uris = [
          "turns:kropki.org:${builtins.toString config.services.coturn.listening-port}?transport=udp"
          "turns:kropki.org:${builtins.toString config.services.coturn.listening-port}?transport=tcp"
          "turn:kropki.org:${builtins.toString config.services.coturn.listening-port}?transport=udp"
          "turn:kropki.org:${builtins.toString config.services.coturn.listening-port}?transport=tcp"
        ];
        turn_secret_file = config.age.secrets.coturn.path or "/secrets/coturn";
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
    nginx.virtualHosts = {
      "kropki.org".locations = {
        "/_matrix" = {
          proxyPass = "http://localhost:6167";
          proxyWebsockets = true;
        };
        "= /.well-known/matrix/server" = {
          return =
            let
              json = {
                "m.server" = "kropki.org";
              };
            in
            "200 '${builtins.toJSON json}'";
          extraConfig = "add_header Content-Type application/json;";
        };
        "= /.well-known/matrix/client" = {
          return =
            let
              json = {
                "m.homeserver".base_url = "https://kropki.org";
                "org.matrix.msc3575.proxy".url = "https://kropki.org";
                "org.matrix.msc4143.rtc_foci" = [
                  {
                    type = "livekit";
                    livekit_service_url = "https://kropki.org/livekit/jwt";
                  }
                ];
              };
            in
            "200 '${builtins.toJSON json}'";
          extraConfig = "add_header Content-Type application/json;";
        };
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

  users.users.continuwuity.extraGroups = [ "turnserver" ];

  age.secrets.continuwuity = {
    file = ../../secrets/continuwuity.age;
    owner = "continuwuity";
    group = "continuwuity";
  };
}
