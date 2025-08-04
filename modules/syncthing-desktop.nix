{
  pkgs,
  ...
}:

{
  # for plasma integration
  environment.systemPackages = [ pkgs.syncthingtray ];
  home-manager.users.kurnevsky.services.syncthing.tray = {
    enable = true;
    package = pkgs.syncthingtray;
  };
}
