{ pkgs, ... }: {
  boot.cleanTmpDir = true;

  environment.systemPackages = [ pkgs.git ];

  networking.hostName = "digitalocean";
}
