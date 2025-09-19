{
  config,
  ...
}:

{
  services = {
    livekit = {
      enable = true;
      openFirewall = true;
      keyFile = config.age.secrets.livekit.path or "/secrets/livekit";
      settings.prometheus_port = 31049;
    };

    lk-jwt-service = {
      enable = true;
      port = 43976;
      livekitUrl = "wss://kropki.org/livekit/sfu";
      keyFile = config.age.secrets.livekit.path or "/secrets/livekit";
    };

    nginx.virtualHosts."kropki.org".locations = {
      "/livekit/jwt/".proxyPass = "http://localhost:${toString config.services.lk-jwt-service.port}/";
      "/livekit/sfu/" = {
        extraConfig = ''
          proxy_buffering off;
        '';
        proxyPass = "http://localhost:${toString config.services.livekit.settings.port}/";
        proxyWebsockets = true;
      };
    };
  };

  systemd.services.lk-jwt-service.environment.LIVEKIT_FULL_ACCESS_HOMESERVERS = "kropki.org";

  age.secrets.livekit.file = ../../secrets/livekit.age;
}
