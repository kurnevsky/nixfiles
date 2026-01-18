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
        APP_NAME = "Kropki ID";
        APP_URL = "https://id.kropki.org";
        TRUST_PROXY = true;
        DB_CONNECTION_STRING = "postgres:///pocket-id?host=/var/run/postgresql";
        UI_CONFIG_DISABLED = true;
        HOST = "127.0.0.1";
        METRICS_ENABLED = true;
        OTEL_METRICS_EXPORTER = "prometheus";
      };
      credentials.ENCRYPTION_KEY = config.age.secrets.pocket-id.path or "/secrets/pocket-id";
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

  age.secrets.pocket-id = {
    file = ../../secrets/pocket-id.age;
    owner = "pocket-id";
    group = "pocket-id";
  };
}
