{
  pkgs,
  config,
  ...
}:

{
  services = {
    miniflux = {
      enable = true;
      config = {
        LISTEN_ADDR = "127.0.0.1:34449";
        BASE_URL = "https://rss.kropki.org";
        HTTPS = 1;
        FORCE_REFRESH_INTERVAL = 15;
        POLLING_FREQUENCY = 30;
        POLLING_SCHEDULER = "entry_frequency";
        SCHEDULER_ENTRY_FREQUENCY_FACTOR = 2;
        CREATE_ADMIN = 0;
        DISABLE_LOCAL_AUTH = 1;
        OAUTH2_PROVIDER = "oidc";
        OAUTH2_CLIENT_ID = "b44fec60-0c21-4b1b-9a32-472b547a5341";
        OAUTH2_CLIENT_SECRET_FILE = config.age.secrets.miniflux.path or "/secrets/miniflux";
        OAUTH2_REDIRECT_URL = "https://rss.kropki.org/oauth2/oidc/callback";
        OAUTH2_OIDC_DISCOVERY_ENDPOINT = "https://id.kropki.org";
        OAUTH2_USER_CREATION = 1;
        METRICS_COLLECTOR = 1;
      };
    };

    nginx.virtualHosts."rss.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations = {
        "/".proxyPass = "http://localhost:34449";
        "= /metrics".return = 403;
        "/reactflux/" = {
          alias = "${pkgs.callPackage ./reactflux.nix { baseurl = "/reactflux"; }}/";
          index = "index.html";
          tryFiles = "$uri uri/ /index.html =404";
          extraConfig = "expires 24h;";
        };
        "= /reactflux".return = "301 $request_uri/";
      };
    };
  };

  security.acme.certs."rss.kropki.org".group = "acme";

  age.secrets.miniflux = {
    file = ../../secrets/miniflux.age;
    owner = "miniflux";
    group = "miniflux";
  };
}
