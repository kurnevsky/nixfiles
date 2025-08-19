{
  config,
  ...
}:

{
  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    checkConfig = "syntax-only";
    scrapeConfigs = [
      {
        job_name = "node";
        static_configs = [
          {
            targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ];
          }
        ];
      }
      {
        job_name = "postgres";
        static_configs = [
          {
            targets = [ "localhost:${toString config.services.prometheus.exporters.postgres.port}" ];
          }
        ];
      }
      {
        job_name = "miniflux";
        static_configs = [
          {
            targets = [ "localhost:34449" ];
          }
        ];
      }
      {
        job_name = "wakapi";
        metrics_path = "/api/metrics";
        bearer_token_file = config.age.secrets.prometheus-wakapi.path or "/secrets/prometheus-wakapi";
        static_configs = [
          {
            targets = [ "localhost:3000" ];
          }
        ];
      }
    ];
    exporters = {
      node = {
        enable = true;
        listenAddress = "127.0.0.1";
        enabledCollectors = [ "systemd" ];
      };
      postgres = {
        enable = true;
        runAsLocalSuperUser = true;
        listenAddress = "127.0.0.1";
      };
    };
  };

  services.nginx.virtualHosts."prometheus.kropki.org" = {
    http3 = true;
    quic = true;
    enableACME = true;
    forceSSL = true;
    kTLS = true;
    locations."/".proxyPass = "http://localhost:${builtins.toString config.services.prometheus.port}";
  };

  services.oauth2-proxy.nginx.virtualHosts."prometheus.kropki.org" = { };

  age.secrets.prometheus-wakapi = {
    file = ../../secrets/prometheus-wakapi.age;
    owner = "prometheus";
    group = "prometheus";
  };
}
