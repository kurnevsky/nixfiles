{
  services = {
    ntfy-sh = {
      enable = true;
      settings = {
        behind-proxy = true;
        base-url = "https://ntfy.kropki.org";
        auth-default-access = "deny-all";
        enable-login = true;
        require-login = true;
      };
    };

    nginx.virtualHosts."ntfy.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations."/" = {
        proxyPass = "http://localhost:2586";
        proxyWebsockets = true;
      };
    };
  };
}
