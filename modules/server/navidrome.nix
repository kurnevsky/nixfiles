{
  services = {
    navidrome = {
      enable = true;
      settings = {
        BaseUrl = "https://music.kropki.org";
        ReverseProxyWhitelist = "::1/128,127.0.0.1/8";
        ReverseProxyUserHeader = "X-Preferred-Username";
        "Scanner.PurgeMissing" = "always";
        EnableTranscodingConfig = true;
        "Prometheus.Enabled" = true;
      };
    };

    nginx.virtualHosts."music.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations = {
        "/" = {
          proxyPass = "http://localhost:4533";
          extraConfig = ''
            auth_request_set $preferred_username $upstream_http_x_auth_request_preferred_username;
            proxy_set_header X-Preferred-Username $preferred_username;
          '';
        };
        "/share/" = {
          proxyPass = "http://localhost:4533";
          extraConfig = "auth_request off;";
        };
        "/rest/" = {
          proxyPass = "http://localhost:4533";
          extraConfig = "auth_request off;";
        };
        "= /metrics".return = 403;
      };
    };

    oauth2-proxy.nginx.virtualHosts."music.kropki.org" = { };
  };
}
