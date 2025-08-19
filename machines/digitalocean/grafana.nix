{
  config,
  ...
}:

{
  services = {
    postgresql = {
      ensureDatabases = [
        "grafana"
      ];
      ensureUsers = [
        {
          name = "grafana";
          ensureDBOwnership = true;
        }
      ];
    };

    grafana = {
      enable = true;
      settings = {
        server = {
          http_port = 3001;
          root_url = "https://grafana.kropki.org";
        };
        database = {
          type = "postgres";
          host = "/var/run/postgresql";
          user = "grafana";
        };
        auth.disable_login_form = true;
        "auth.basic".enabled = false;
        "auth.generic_oauth" = {
          enabled = true;
          allow_sign_up = true;
          auto_login = true;
          client_id = "afd931f6-4d16-41c5-9a2a-b68a944917b4";
          client_secret = "$__file{${config.age.secrets.grafana.path or "/secrets/grafana"}}";
          scopes = "openid profile email groups";
          auth_url = "https://id.kropki.org/authorize";
          token_url = "https://id.kropki.org/api/oidc/token";
          api_url = "https://id.kropki.org/api/oidc/userinfo";
          use_pkce = true;
          use_refresh_token = true;
        };
      };
      provision = {
        enable = true;
        datasources.settings.datasources = [
          {
            name = "Prometheus";
            type = "prometheus";
            url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            isDefault = true;
            editable = false;
          }
        ];
      };
    };

    nginx.virtualHosts."grafana.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations."/" = {
        proxyWebsockets = true;
        proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
      };
    };
  };

  age.secrets.grafana = {
    file = ../../secrets/grafana.age;
    owner = "grafana";
    group = "grafana";
  };
}
