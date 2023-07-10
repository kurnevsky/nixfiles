{ pkgs, ... }:

{
  environment.packages = with pkgs; [ git mc nano ];

  system.stateVersion = "23.05";

  home-manager.config = { pkgs, ... }: { home.stateVersion = "23.05"; };
}
