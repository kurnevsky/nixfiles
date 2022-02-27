{ pkgs, ... }: {
  boot.cleanTmpDir = true;

  networking.hostName = "digitalocean";
}
