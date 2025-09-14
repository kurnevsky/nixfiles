{
  pkgs,
  config,
  ...
}:

{
  environment.systemPackages = with pkgs; [ cifs-utils ];

  fileSystems."/mnt" = {
    device = "//u492306.your-storagebox.de/backup";
    fsType = "cifs";
    options =
      let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";
      in
      [ "${automount_opts},credentials=${config.age.secrets.storage.path or "/secrets/storage"}" ];
  };

  age.secrets.storage.file = ../../secrets/storage.age;

  services.navidrome.settings.MusicFolder = "/mnt/music";
}
