{ pkgs, ... }:

{
  environment.packages = with pkgs; [ git lsof mc nano openssh gnupg ];

  system.stateVersion = "23.05";

  home-manager.config = args@{ pkgs, ... }:
    (import ../../modules/common-home-config.nix args) // {
      home.stateVersion = "23.05";
    };
}
