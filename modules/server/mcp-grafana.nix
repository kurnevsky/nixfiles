{
  pkgs,
  config,
  ...
}:

{
  services.nginx.virtualHosts."grafana.kropki.org".locations."= /mcp" = {
    proxyPass = "http://localhost:34451";
    # it would override the Host header below
    recommendedProxySettings = false;
    extraConfig = ''
      # it listens on loopback and rejects non-loopback Host headers
      # as a DNS rebinding protection
      proxy_set_header Host localhost;
      proxy_set_header "Connection" "";
      proxy_set_header X-Real-IP $remote_addr;
      proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
      proxy_set_header X-Forwarded-Proto $scheme;
      proxy_buffering off;
    '';
  };

  systemd.services.mcp-grafana = {
    description = "Grafana MCP server";
    after = [ "grafana.service" ];
    wants = [ "grafana.service" ];
    wantedBy = [ "multi-user.target" ];
    environment.GRAFANA_URL = "http://${config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.mcp-grafana}/bin/mcp-grafana -t streamable-http -address 127.0.0.1:34451 -endpoint-path /mcp -allowed-hosts localhost -disable-write -disable-admin";
      # the bearer token is taken from MCP_GRAFANA_SERVER_TOKEN
      EnvironmentFile = "${config.age.secrets.mcp-grafana.path or "/secrets/mcp-grafana"}";
    };
  };

  age.secrets.mcp-grafana.file = ../../secrets/mcp-grafana.age;
}
