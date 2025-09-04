{
  config,
  ...
}:

{
  networking = {
    nat.internalInterfaces = [
      "icmp"
    ];
    firewall.trustedInterfaces = [
      "icmp"
    ];
  };

  services.hans.server = {
    enable = true;
    ip = "172.18.43.0";
    extraConfig = "-d icmp -m 1200";
    passwordFile = config.age.secrets.hans.path or "/secrets/hans";
  };

  users = {
    users.hans = {
      group = "hans";
      isSystemUser = true;
    };
    groups.hans = { };
  };

  age.secrets.hans = {
    file = ../../secrets/hans.age;
    owner = "hans";
    group = "hans";
  };
}
