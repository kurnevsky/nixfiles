{ pkgs, ... }:

{
  system.stateVersion = "22.11";

  home-manager.users = {
    root.home.stateVersion = "22.11";
    kurnevsky.home.stateVersion = "22.11";
  };
}
