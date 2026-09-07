{
  pkgs,
  config,
  ...
}:

{
  services.nginx.virtualHosts."searx.kropki.org".locations."= /mcp" = {
    proxyPass = "http://localhost:34454";
    # it would override the Host header below
    recommendedProxySettings = false;
    extraConfig = ''
      # the mcp server checks the bearer token on its own
      auth_request off;
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

  systemd.services.mcp-searxng = {
    description = "SearXNG MCP server";
    after = [ "uwsgi.service" ];
    wants = [ "uwsgi.service" ];
    wantedBy = [ "multi-user.target" ];
    environment = {
      MCP_HTTP_HOST = "127.0.0.1";
      MCP_HTTP_PORT = "34454";
      # it enables the bearer token authentication
      MCP_HTTP_HARDEN = "true";
      MCP_HTTP_ALLOWED_ORIGINS = "https://searx.kropki.org";
      # a single nginx hop in front
      MCP_HTTP_TRUST_PROXY = "1";
      SEARXNG_URL = "http://${config.services.uwsgi.instance.vassals.searx.http}";
    };
    serviceConfig = {
      Restart = "on-failure";
      RestartSec = 5;
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.mcp-searxng}/bin/mcp-searxng";
      # the bearer token is taken from MCP_HTTP_AUTH_TOKEN
      EnvironmentFile = "${config.age.secrets.mcp-searxng.path or "/secrets/mcp-searxng"}";
    };
  };

  age.secrets.mcp-searxng.file = ../../secrets/mcp-searxng.age;
}
