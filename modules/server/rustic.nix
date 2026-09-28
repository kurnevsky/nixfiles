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
      path = [
        pkgs.rclone
        pkgs.oath-toolkit
      ];
      environment = {
        RCLONE_CONFIG_BACKUPS_TYPE = "mega";
        RCLONE_CONFIG_BACKUPS_HARD_DELETE = "true";
      };
      # Each rustic run spawns rclone, which logs in anew, so it needs a fresh TOTP code
      script = ''
        RCLONE_CONFIG_BACKUPS_2FA="$(oathtool --totp -b "$MEGA_TOTP_SECRET")" ${pkgs.rustic}/bin/rustic backup --set-compression 16
        RCLONE_CONFIG_BACKUPS_2FA="$(oathtool --totp -b "$MEGA_TOTP_SECRET")" ${pkgs.rustic}/bin/rustic forget
      '';
      serviceConfig = {
        Type = "oneshot";
        User = "root";
        EnvironmentFile = "${config.age.secrets.rustic-rclone.path or "/secrets/rustic-rclone"}";
      };
    };
  };

  home-manager.users.root.xdg.configFile."rustic/rustic.toml".text = ''
    [global]
    no-progress = true
    opentelemetry = "http://localhost:${builtins.toString config.services.prometheus.port}/api/v1/otlp/v1/metrics"

    [repository]
    repository = "rclone:backups:rustic"
    password-file = "${config.age.secrets.rustic.path or "/secrets/rustic"}"

    [forget]
    keep-last = 10
    prune = true

    [backup]
    init = true

    [[backup.snapshots]]
    sources = ["/var/lib"]
  '';

  age.secrets = {
    rustic.file = ../../secrets/rustic.age;
    rustic-rclone.file = ../../secrets/rustic-rclone.age;
  };
}
