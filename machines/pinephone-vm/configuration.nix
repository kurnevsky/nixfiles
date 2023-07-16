{ ... }:

{
  mobile.boot.stage-1.kernel.additionalModules = [ "btrfs" ];

  system.stateVersion = "23.05";

  home-manager.users = {
    root.home.stateVersion = "23.05";
    kurnevsky.home.stateVersion = "23.05";
  };
}
