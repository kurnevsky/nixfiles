{
  pkgs,
  ...
}:

{
  networking.firewall = {
    allowedTCPPorts = [
      # HTTP
      80
      # HTTPS
      443
    ];
    allowedUDPPorts = [
      # QUIC
      443
    ];
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "kurnevsky@gmail.com";
    certs."kropki.org".group = "acme";
  };

  users.groups.acme.members = [
    "nginx"
  ];

  services.nginx = {
    enable = true;
    package = pkgs.nginxMainline;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    proxyTimeout = "300s";
    virtualHosts."kropki.org" = {
      default = true;
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations."/static/" = {
        alias = "/srv/www/static/";
        tryFiles = "$uri =404";
        extraConfig = "expires 24h;";
      };
    };
  };
}
