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

  services.turn-rs = {
    enable = true;
    settings = {
      auth.static_auth_secret = "$TURN_SECRET";
      api.bind = "127.0.0.1:3002";
      turn = {
        interfaces = [
          {
            bind = "0.0.0.0:47354";
            external = "49.12.217.127:47354";
            transport = "udp";
          }
          {
            bind = "0.0.0.0:47354";
            external = "49.12.217.127:47354";
            transport = "tcp";
          }
        ];
        realm = "turn.kropki.org";
      };
    };
    secretFile = config.age.secrets.turn.path or "/secrets/turn";
  };

  age.secrets.turn.file = ../../secrets/turn.age;
}
