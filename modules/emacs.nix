users:

{ lib, pkgs, ... }:

let
  emacs = pkgs.callPackage ./emacs/package.nix { emacs = pkgs.emacs30-pgtk; };
in
lib.mkMerge [
  {
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package = emacs;
    };

    environment.systemPackages = with pkgs; [ emacs-lsp-booster ];
  }
  (import ./for-all-home-users.nix users (import ./emacs/home.nix emacs))
]
