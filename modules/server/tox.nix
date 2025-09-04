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
    keysFile = config.age.secrets.tox.path or "/secrets/tox";
    tcpAddresses = [ ];
    lanDiscovery = false;
    motd = "Hi from tox-rs!";
  };

  systemd.services.tox-node.serviceConfig.SupplementaryGroups = "secrets-tox";

  users.groups.secrets-tox = { };

  age.secrets.tox = {
    file = ../../secrets/tox.age;
    mode = "440";
    group = "secrets-tox";
  };
}
