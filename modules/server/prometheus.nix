{
  config,
  ...
}:

{
  services.prometheus = {
    enable = true;
    listenAddress = "127.0.0.1";
    checkConfig = "syntax-only";
    extraFlags = [ "--web.enable-otlp-receiver" ];
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
      {
        job_name = "stalwart";
        metrics_path = "/metrics/prometheus";
        # Add a 'stalwart' prefix to every metric
        metric_relabel_configs = [
          {
            source_labels = [ "__name__" ];
            separator = "_";
            regex = "(.*)";
            replacement = "stalwart_$1";
            target_label = "__name__";
            action = "replace";
          }
        ];
        static_configs = [
          {
            targets = [ "localhost:30452" ];
          }
        ];
      }
      {
        job_name = "pocket-id";
        static_configs = [
          {
            targets = [ "localhost:9464" ];
          }
        ];
      }
      {
        job_name = "prosody";
        static_configs = [
          {
            targets = [ "localhost:5280" ];
          }
        ];
      }
      {
        job_name = "coturn";
        # Add a 'coturn' prefix to every metric
        metric_relabel_configs = [
          {
            source_labels = [ "__name__" ];
            separator = "_";
            regex = "(.*)";
            replacement = "coturn_$1";
            target_label = "__name__";
            action = "replace";
          }
        ];
        static_configs = [
          {
            targets = [ "localhost:9641" ];
          }
        ];
      }
      {
        job_name = "forgejo";
        static_configs = [
          {
            targets = [ "localhost:3003" ];
          }
        ];
      }
      {
        job_name = "navidrome";
        static_configs = [
          {
            targets = [ "localhost:4533" ];
          }
        ];
      }
      {
        job_name = "livekit";
        static_configs = [
          {
            targets = [ "localhost:31049" ];
          }
        ];
      }
      {
        job_name = "air-1";
        scrape_interval = "10s";
        static_configs = [
          {
            targets = [ "pc:9926" ];
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
