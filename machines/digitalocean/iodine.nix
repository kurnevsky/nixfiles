{
  config,
  ...
}:

{
  networking = {
    nat.internalInterfaces = [
      "dns0"
    ];
    firewall.trustedInterfaces = [
      "dns0"
    ];
  };

  services.iodine.server = {
    enable = true;
    ip = "172.18.42.1/24";
    domain = "i.kropki.org";
    extraConfig = "-n 82.196.15.215";
    passwordFile = config.age.secrets.iodine.path or "/secrets/iodine";
  };

  age.secrets.iodine = {
    file = ../../secrets/iodine.age;
    owner = "iodined";
    group = "iodined";
  };
}
