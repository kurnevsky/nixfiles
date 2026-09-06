{
  pkgs,
  config,
  ...
}:

{
  services.nginx = {
    # the mcp server doesn't support authentication on its own, so the token is
    # checked here - the secret contains a map entry like `"Bearer xxx" 1;` to
    # keep the token out of the nix store
    appendHttpConfig = ''
      map $http_authorization $prometheus_mcp_authorized {
        default 0;
        include ${config.age.secrets.prometheus-mcp.path or "/secrets/prometheus-mcp"};
      }
    '';

    virtualHosts."prometheus.kropki.org".locations."= /mcp" = {
      proxyPass = "http://localhost:34452";
      # it would override the Host header below
      recommendedProxySettings = false;
      extraConfig = ''
        auth_request off;
        if ($prometheus_mcp_authorized = 0) {
          return 401;
        }
        # it listens on loopback and rejects non-loopback Host headers
        # as a DNS rebinding protection
        proxy_set_header Host localhost;
        proxy_set_header "Connection" "";
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        # it would be forwarded to prometheus otherwise
        proxy_set_header Authorization "";
        proxy_buffering off;
      '';
    };
  };

  systemd.services.prometheus-mcp = {
    description = "Prometheus MCP server";
    after = [ "prometheus.service" ];
    wants = [ "prometheus.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${
        pkgs.callPackage ./prometheus-mcp-server.nix { }
      }/bin/prometheus-mcp-server --mcp.transport=http --web.listen-address=127.0.0.1:34452 --prometheus.url=http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
    };
  };

  age.secrets.prometheus-mcp = {
    file = ../../secrets/prometheus-mcp.age;
    owner = config.services.nginx.user;
    group = config.services.nginx.group;
  };
}
