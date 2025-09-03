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
      "kropki.org".locations."/_matrix" = {
        proxyPass = "http://localhost:6167";
        proxyWebsockets = true;
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

  age.secrets.continuwuity = {
    file = ../../secrets/continuwuity.age;
    owner = "continuwuity";
    group = "continuwuity";
  };
}
