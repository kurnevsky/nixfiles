{
  config,
  ...
}:

{
  networking.firewall = {
    allowedTCPPorts = [
      47354
      47356
    ];
    allowedUDPPorts = [
      47354
      47356
    ];
  };

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    realm = "kropki.org";
    listening-port = 47354;
    tls-listening-port = 47356;
    cert = "${config.security.acme.certs."kropki.org".directory}/fullchain.pem";
    pkey = "${config.security.acme.certs."kropki.org".directory}/key.pem";
    static-auth-secret-file = config.age.secrets.coturn.path or "/secrets/coturn";
    extraConfig = "prometheus";
  };

  age.secrets.coturn = {
    file = ../../secrets/coturn.age;
    owner = "turnserver";
    group = "turnserver";
  };
}
