{
  config,
  ...
}:

{
  networking.firewall = {
    allowedTCPPorts = [
      47354
    ];
    allowedUDPPorts = [
      47354
    ];
  };

  services.coturn = {
    enable = true;
    use-auth-secret = true;
    realm = "turn.kropki.org";
    listening-port = 47354;
    no-tls = true;
    no-dtls = true;
    static-auth-secret-file = config.age.secrets.coturn.path or "/secrets/coturn";
  };

  age.secrets.coturn = {
    file = ../../secrets/coturn.age;
    owner = "turnserver";
    group = "turnserver";
  };
}
