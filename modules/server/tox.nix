{
  config,
  ...
}:

{
  networking.firewall = {
    allowedTCPPorts = [
      # Tox
      33445
    ];
    allowedUDPPorts = [
      # Tox
      33445
    ];
  };

  services.tox-node = {
    enable = true;
    keysFile = "/run/credentials/tox-node.service/keys";
    tcpAddresses = [ ];
    lanDiscovery = false;
    motd = "Hi from tox-rs!";
  };

  systemd.services.tox-node.serviceConfig.LoadCredential = [
    "keys:${config.age.secrets.tox.path or "/secrets/tox"}"
  ];

  age.secrets.tox.file = ../../secrets/tox.age;
}
