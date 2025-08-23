{
  config,
  ...
}:

{
  services = {
    scrutiny = {
      enable = true;
      collector.enable = false;
      settings.web.listen = {
        host = "127.0.0.1";
        port = 17074;
      };
    };

    nginx.virtualHosts."scrutiny.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations =
        let
          proxyPass = "http://${config.services.scrutiny.settings.web.listen.host}:${builtins.toString config.services.scrutiny.settings.web.listen.port}";
        in
        {
          "/" = { inherit proxyPass; };
          "/api/" = {
            inherit proxyPass;
            extraConfig = "satisfy any;";
            basicAuthFile = config.age.secrets.scrutiny.path or "/secrets/scrutiny";
          };
        };
    };

    oauth2-proxy.nginx.virtualHosts."scrutiny.kropki.org" = { };
  };

  age.secrets.scrutiny = {
    file = ../../secrets/scrutiny.age;
    owner = "nginx";
    group = "nginx";
  };
}
