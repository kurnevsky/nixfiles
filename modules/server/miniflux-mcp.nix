{
  pkgs,
  config,
  ...
}:

{
  services.nginx.virtualHosts."rss.kropki.org".locations."= /mcp" = {
    proxyPass = "http://localhost:34450";
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

  systemd.services.miniflux-mcp = {
    description = "Miniflux MCP server";
    after = [ "miniflux.service" ];
    wants = [ "miniflux.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      MCP_TRANSPORT = "streamable-http";
      MCP_HTTP_ADDR = "127.0.0.1:34450";
      MCP_HTTP_PATH = "/mcp";
      MINIFLUX_URL = "http://${config.services.miniflux.config.LISTEN_ADDR}";
    };
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.callPackage ./miniflux-mcp-server.nix { }}/bin/miniflux-mcp";
      EnvironmentFile = "${config.age.secrets.miniflux-mcp.path or "/secrets/miniflux-mcp"}";
    };
  };

  age.secrets.miniflux-mcp.file = ../../secrets/miniflux-mcp.age;
}
