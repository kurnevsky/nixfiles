users:

{ pkgs, ... }:

let emacs = pkgs.callPackage ./emacs-package.nix { } pkgs.emacs-unstable-pgtk;
in {
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = emacs;
  };

  home-manager = let
    home-config = {
      home.file.".config/emacs/init.el" = {
        source = ./init.el;
        onChange = ''
          export PATH=${pkgs.git}/bin:${pkgs.agda}/bin:"$PATH"
          export EMACSLOADPATH=${pkgs.mu}/share/emacs/site-lisp/mu4e:
          export EMACSNATIVELOADPATH=${pkgs.mu}/share/emacs/native-lisp:
          rm -fv ~/.config/emacs/init.elc
          ${emacs}/bin/emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-byte-compile ~/.config/emacs/init.el
          rm -rfv ~/.config/emacs/eln-cache/
          ${emacs}/bin/emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-native-compile ~/.config/emacs/init.el
        '';
      };
    };
  in {
    users = builtins.listToAttrs (map (user: {
      name = user;
      value = home-config;
    }) users);
  };
}
