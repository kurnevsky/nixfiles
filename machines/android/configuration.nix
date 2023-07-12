{ pkgs, emacs-overlay, ... }:

let
  emacsPkgs = pkgs.extend emacs-overlay;
  emacsWithPackages = emacsPkgs.callPackage ../../modules/emacs-package.nix { }
    emacsPkgs.emacs-unstable-nox;
in {
  environment.packages = with pkgs; [
    git
    lsof
    mc
    nano
    openssh
    gnupg
    emacsWithPackages
  ];

  user.shell = "${pkgs.zsh}/bin/zsh";

  system.stateVersion = "23.05";

  home-manager.config = args@{ pkgs, ... }:
    (import ../../modules/common-home.nix args) // {
      home.stateVersion = "23.05";
    };
}
