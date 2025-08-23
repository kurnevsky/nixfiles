{
  lib,
  config,
  ...
}:

{
  services.scrutiny.collector = {
    enable = true;
    settings.host.id = config.networking.hostName;
  };

  systemd.services.scrutiny-collector = {
    environment.COLLECTOR_API_ENDPOINT = lib.mkForce null;
    serviceConfig.EnvironmentFile = "${config.age.secrets.scrutiny-collector.path or "/secrets/scrutiny-collector"}";
  };

  age.secrets.scrutiny-collector.file = ../secrets/scrutiny-collector.age;
}
