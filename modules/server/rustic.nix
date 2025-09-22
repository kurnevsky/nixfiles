{
  pkgs,
  config,
  ...
}:

{
  systemd = {
    timers.rustic = {
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "daily";
        Persistent = true;
        Unit = "rustic.service";
      };
    };

    services.rustic = {
      script = ''
        ${pkgs.rustic}/bin/rustic backup
        ${pkgs.rustic}/bin/rustic forget
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "root";
      };
    };
  };

  home-manager.users.root.xdg.configFile."rustic/rustic.toml".text = ''
    [global]
    no-progress = true
    opentelemetry = "http://localhost:${builtins.toString config.services.prometheus.port}/api/v1/otlp/v1/metrics"

    [repository]
    repository = "/mnt/rustic"
    password-file = "${config.age.secrets.rustic.path or "/secrets/rustic"}"

    [forget]
    keep-last = 10
    prune = true

    [backup]
    init = true

    [[backup.snapshots]]
    sources = ["/var/lib"]
  '';

  age.secrets.rustic.file = ../../secrets/rustic.age;
}
