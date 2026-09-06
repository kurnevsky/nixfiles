{
  config,
  ...
}:

{
  networking.firewall = {
    allowedTCPPorts = [
      config.services.coturn.listening-port
      config.services.coturn.tls-listening-port
    ];
    allowedUDPPorts = [
      config.services.coturn.listening-port
      config.services.coturn.tls-listening-port
    ];
  };

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    realm = "kropki.org";
    cert = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
    pkey = "${config.security.acme.certs."kropki.org".directory}/key.pem";
    static-auth-secret-file = "/run/credentials/coturn.service/auth-secret";
    extraConfig = "prometheus";
  };

  systemd.services.coturn.serviceConfig.LoadCredential = [
    "auth-secret:${config.age.secrets.coturn.path or "/secrets/coturn"}"
  ];

  age.secrets.coturn.file = ../../secrets/coturn.age;
}
