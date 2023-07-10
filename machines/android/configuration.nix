{ pkgs, ... }:

{
  environment.packages = with pkgs; [ git lsof mc nano openssh gnupg ];

  system.stateVersion = "23.05";

  home-manager.config = { pkgs, ... }: { home.stateVersion = "23.05"; };
}
