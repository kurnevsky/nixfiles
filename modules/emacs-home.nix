emacsPackage:

{ pkgs, ... }:

{
  home.file.".config/emacs/init.el" = {
    source = ./init.el;
    onChange = ''
      export PATH=${pkgs.git}/bin:${pkgs.agda}/bin:"$PATH"
      export EMACSLOADPATH=${pkgs.mu}/share/emacs/site-lisp/mu4e:
      export EMACSNATIVELOADPATH=${pkgs.mu}/share/emacs/native-lisp:
      rm -fv ~/.config/emacs/init.elc
      ${emacsPackage}/bin/emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-byte-compile ~/.config/emacs/init.el
      rm -rfv ~/.config/emacs/eln-cache/
      ${emacsPackage}/bin/emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-native-compile ~/.config/emacs/init.el
    '';
  };
}
