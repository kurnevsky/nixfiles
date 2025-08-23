{
  config,
  ...
}:

{
  services = {
    postgresql = {
      ensureDatabases = [
        "wakapi"
      ];
      ensureUsers = [
        {
          name = "wakapi";
          ensureDBOwnership = true;
        }
      ];
    };

    wakapi = {
      enable = true;
      passwordSaltFile = config.age.secrets.wakapi.path or "/secrets/wakapi";
      settings = {
        server.public_url = "https://waka.kropki.org";
        app.leaderboard_enabled = false;
        db = {
          dialect = "postgres";
          host = "/run/postgresql";
          port = 5432;
          user = "wakapi";
          name = "wakapi";
        };
        security = {
          trusted_header_auth = true;
          trusted_header_auth_key = "X-Preferred-Username";
          trusted_header_auth_allow_signup = true;
          trust_reverse_proxy_ips = "127.0.0.1,::1";
          expose_metrics = true;
        };
      };
      # it adds unwanted authentication
      database.createLocally = false;
    };

    nginx.virtualHosts."waka.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations = {
        "/" = {
          proxyPass = "http://localhost:3000";
          extraConfig = ''
            auth_request_set $preferred_username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header X-Preferred-Username $preferred_username;
          '';
        };
        "/api/" = {
          proxyPass = "http://localhost:3000";
          extraConfig = "auth_request off;";
        };
        "= /api/metrics".return = 403;
      };
    };

    oauth2-proxy.nginx.virtualHosts."waka.kropki.org" = { };
  };

  age.secrets.wakapi.file = ../../secrets/wakapi.age;
}
