emacsPackage:

{ pkgs, ... }:

let
  prepare = ''
    export HOME="$(pwd)"

    mkdir -p ~/.config/emacs/
    cp ${./init.el} ~/.config/emacs/init.el

    mkdir -p ~/roam

    export PATH=${pkgs.git}/bin:${pkgs.agda}/bin:"$PATH"
    export EMACSLOADPATH=${pkgs.mu}/share/emacs/site-lisp/mu4e:
    export EMACSNATIVELOADPATH=${pkgs.mu}/share/emacs/native-lisp:
  '';
  elc-config = pkgs.runCommand "emacs-elc-config" { } ''
    ${prepare}

    ${emacsPackage}/bin/emacs -Q \
      --load ~/.config/emacs/init.el \
      --batch \
      -f 'batch-byte-compile' ~/.config/emacs/init.el

    cp ./.config/emacs/init.elc "$out"
  '';
  eln-config = pkgs.runCommand "emacs-eln-config" { } ''
    ${prepare}

    file="$(
      ${emacsPackage}/bin/emacs -Q \
        --load ~/.config/emacs/init.el \
        --batch \
        --eval "(add-to-list 'native-comp-eln-load-path \"$HOME\")" \
        --eval '(princ (comp-el-to-eln-filename "~/.config/emacs/init.el"))' \
        -f 'batch-native-compile' ~/.config/emacs/init.el
    )"

    cp "$file" "$out"
  '';
in {
  home.file = {
    ".config/emacs/early-init.el".source = ./early-init.el;
    ".config/emacs/init.el".source = ./init.el;
    ".config/emacs/init.elc" = {
      source = elc-config;
      onChange = ''
        rm -rf ~/.config/emacs/eln-cache
        dest="$(
          ${emacsPackage}/bin/emacs -Q \
            --batch \
            --eval '(princ (comp-el-to-eln-filename "~/.config/emacs/init.el"))'
        )"
        mkdir -p "$(dirname "$dest")"
        ln -s ${eln-config} "$dest"
      '';
    };
  };
}
