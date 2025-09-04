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
    ./miniflux.nix
    ./oauth2-proxy.nix
    ./wakapi.nix
    ./prometheus.nix
    ./grafana.nix
    ./scrutiny.nix
    ./wireguard.nix
    ./tox.nix
    ./matrix.nix
    ./yggdrasil.nix
    ./syncthing.nix
    ./hans.nix
    ./iodine.nix
    ./nginx.nix
    ./kropki.nix
    ./tt-rss.nix
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
