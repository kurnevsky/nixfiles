{ pkgs, emacs-overlay, ... }:

let
  emacsPkgs = pkgs.extend emacs-overlay;
  emacsWithPackages = emacsPkgs.callPackage ../../modules/emacs-package.nix { }
    emacsPkgs.emacs-unstable-nox;
in {
  nix = {
    substituters =
      [ "https://cachix.cachix.org" "https://nix-community.cachix.org" ];
    trustedPublicKeys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      # local keys
      "pc:NVXsu6EBpoB+a+W2ZTkpTWD9Vj5nJtjYsAfe8zTzDKE="
      "evo:aBUuIOLObUPUBWBIW0XGyMZW2Wor5z3ZNtaRynZ35UY="
      "digitalocean:jFnutzjWmSNY5q/frkSkijlCh8GfdNa1Mpm5Y0N15sQ="
    ];
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

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
