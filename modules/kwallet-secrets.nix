{ pkgs, ... }:

{
  systemd.user.services."dbus-org.freedesktop.secrets.kwallet" = {
    description = "Allow KWallet to be D-Bus activated for the generic org.freedesktop.secrets API";

    serviceConfig = {
      Type = "dbus";
      ExecStart = "${pkgs.kdePackages.kwallet}/bin/kwalletd6";
      BusName = "org.freedesktop.secrets";
    };

    aliases = [
      "dbus-org.freedesktop.secrets.service"
      "dbus-org.kde.kwalletd5.service"
    ];
  };

  services.dbus.packages = [
    (pkgs.writeTextFile {
      name = "org.freedesktop.secrets.kwallet.service";
      destination = "/share/dbus-1/services/org.freedesktop.secrets.service";
      text = ''
        [D-BUS Service]
        Name=org.freedesktop.secrets
        SystemdService=dbus-org.freedesktop.secrets.service
      '';
    })
  ];
}
