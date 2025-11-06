{
  pkgs,
  ...
}:

{
  imports = [
    ./shadowsocks-server.nix
    ./websocat-ssh-server.nix
    ./websocat-wg-server.nix
    ./pocket-id.nix
    ./stalwart.nix
    ./wkd.nix
    ./miniflux.nix
    ./oauth2-proxy.nix
    ./wakapi.nix
    ./prometheus.nix
    ./grafana.nix
    ./scrutiny.nix
    ./wireguard.nix
    ./tox.nix
    ./coturn.nix
    ./livekit.nix
    ./matrix.nix
    ./mautrix-whatsapp.nix
    ./prosody.nix
    ./yggdrasil.nix
    ./syncthing.nix
    ./hans.nix
    ./iodine.nix
    ./ntfy.nix
    ./forgejo.nix
    ./navidrome.nix
    ./nginx.nix
    ./kropki.nix
    ./i2pd.nix
    ./rustic.nix
    ./anki-sync-server.nix
  ];

  boot.tmp.cleanOnBoot = true;

  networking = {
    nat.enable = true;
    firewall.enable = true;
  };

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_17;
  };

  age.secrets.github.file = ../../secrets/github.age;
}
