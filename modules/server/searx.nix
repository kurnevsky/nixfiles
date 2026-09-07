{
  config,
  ...
}:

{
  services = {
    searx = {
      enable = true;
      # run in uwsgi behind nginx as it's recommended for production
      configureNginx = true;
      domain = "searx.kropki.org";
      environmentFile = config.age.secrets.searx.path or "/secrets/searx";
      uwsgiConfig = {
        socket = "/run/searx/uwsgi.sock";
        chmod-socket = "660";
        # an additional loopback listener for the mcp server that can't pass
        # through oauth2 - nginx uses the socket above
        http = "127.0.0.1:39281";
        # the requests are logged by nginx anyway
        disable-logging = true;
      };
      settings = {
        general = {
          instance_name = "Searx";
          donation_url = false;
          contact_url = false;
          privacypolicy_url = false;
        };
        server = {
          secret_key = "$SEARX_SECRET_KEY";
          # it's not publicly available - no need for rate limiting
          limiter = false;
          public_instance = false;
          image_proxy = true;
        };
        ui.default_locale = "en";
        search = {
          autocomplete = "duckduckgo";
          favicon_resolver = "duckduckgo";
          formats = [
            "html"
            "json"
          ];
        };
      };
    };

    nginx.virtualHosts."searx.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
    };

    oauth2-proxy.nginx.virtualHosts."searx.kropki.org" = { };
  };

  age.secrets.searx.file = ../../secrets/searx.age;
}
