{
  pkgs,
  config,
  ...
}:

{
  services = {
    postgresql = {
      ensureDatabases = [ "pocket-id" ];
      ensureUsers = [
        {
          name = "pocket-id";
          ensureDBOwnership = true;
        }
      ];
    };

    pocket-id = {
      enable = true;
      settings = {
        ANALYTICS_DISABLED = true;
        APP_URL = "https://id.kropki.org";
        TRUST_PROXY = true;
        DB_PROVIDER = "postgres";
        DB_CONNECTION_STRING = "user=pocket-id dbname=pocket-id host=/run/postgresql";
        UI_CONFIG_DISABLED = true;
        HOST = "127.0.0.1";
        METRICS_ENABLED = true;
        OTEL_METRICS_EXPORTER = "prometheus";
      };
    };

    nginx.virtualHosts."id.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations."/".proxyPass = "http://localhost:1411";
    };
  };

  # By default pocket-id uses RSA256, but it can use EdDSA if generated externally
  systemd.services = {
    pocket-id.serviceConfig.RestrictAddressFamilies = [ "AF_UNIX" ];
    pocket-id-generate-keys = {
      description = "Pocket ID keys generation";
      wantedBy = [ "pocket-id.service" ];
      before = [ "pocket-id.service" ];
      serviceConfig = {
        Type = "oneshot";
        User = config.services.pocket-id.user;
        Group = config.services.pocket-id.group;
      };
      script =
        let
          directory = "${config.services.pocket-id.dataDir}/data/keys";
        in
        ''
          if [ ! -d "${directory}" ]; then
            mkdir -p "${directory}"
            cd "${directory}"
            ${pkgs.step-cli}/bin/step crypto jwk create jwt_public_key.json jwt_private_key.json \
              --kty=OKP \
              --alg=EdDSA \
              --use=sig \
              --crv=Ed25519 \
              --no-password \
              --insecure
          fi
        '';
    };
  };
}
