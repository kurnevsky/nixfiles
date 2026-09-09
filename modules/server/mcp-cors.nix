{
  ...
}:

let
  cors = ''
    # an empty value makes nginx skip the header, so a foreign origin gets no
    # cors headers at all
    add_header Access-Control-Allow-Origin $mcp_cors_origin always;
    add_header Access-Control-Allow-Methods "GET, POST, DELETE, OPTIONS" always;
    add_header Access-Control-Allow-Headers "Authorization, Content-Type, Accept, Mcp-Session-Id, Mcp-Protocol-Version, Last-Event-ID" always;
    # the session id is needed to keep talking to the same streamable http session
    add_header Access-Control-Expose-Headers "Mcp-Session-Id, Mcp-Protocol-Version" always;
    add_header Access-Control-Max-Age 86400 always;
    add_header Vary Origin always;
    # credentials are deliberately not allowed - the bearer token is passed
    # explicitly by the client, and the oauth2 cookie must stay unusable here
    #
    # a preflight never carries the token, so it can't pass the authentication
    # below and has to be answered by nginx itself
    if ($request_method = OPTIONS) {
      return 204;
    }
    proxy_hide_header Access-Control-Allow-Origin;
    proxy_hide_header Access-Control-Allow-Methods;
    proxy_hide_header Access-Control-Allow-Headers;
    proxy_hide_header Access-Control-Expose-Headers;
    proxy_hide_header Access-Control-Allow-Credentials;
    proxy_hide_header Access-Control-Max-Age;
    # the origin is validated above already, and some of the mcp servers reject
    # every request carrying an origin as a dns rebinding protection
    proxy_set_header Origin "";
  '';
in

{
  services.nginx = {
    # any loopback origin regardless of the port - a remote page can't forge
    # one, so this only widens the access to software running locally anyway
    appendHttpConfig = ''
      map $http_origin $mcp_cors_origin {
        default "";
        "~*^http://(localhost|127\.0\.0\.1|\[::1\])(:[0-9]+)?$" $http_origin;
      }
    '';

    virtualHosts = {
      "rss.kropki.org".locations."= /mcp".extraConfig = cors;
      "prometheus.kropki.org".locations."= /mcp".extraConfig = cors;
      "grafana.kropki.org".locations."= /mcp".extraConfig = cors;
      "searx.kropki.org".locations."= /mcp".extraConfig = cors;
    };
  };
}
