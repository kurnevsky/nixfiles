users:

{ lib, pkgs, ... }:

let
  emacs =
    pkgs.callPackage ./emacs/package.nix { emacs = pkgs.emacs-unstable-pgtk; };
in lib.mkMerge [
  {
    services.emacs = {
      enable = true;
      defaultEditor = true;
      package = emacs;
    };
  }
  (import ./for-all-home-users.nix users (import ./emacs/home.nix emacs))
]
