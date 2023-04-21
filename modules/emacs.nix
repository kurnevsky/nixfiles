users:

{ pkgs, lib, ... }:

{
  services.emacs = {
    enable = true;
    defaultEditor = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = pkgs.emacs-gtk;
      alwaysEnsure = true;
      extraEmacsPackages = epkgs: [
        (pkgs.stdenv.mkDerivation {
          name = "hexrgb.el";
          src = pkgs.fetchurl {
            url = "https://www.emacswiki.org/emacs/download/hexrgb.el";
            sha256 = "sha256-qFKdT3IjhUAgNaCVTjCe2cT8rOqfPSdbVCL14/JCC6I=";
          };
          unpackCmd = "mkdir el && cp $curSrc el/hexrgb.el";
          buildPhase =
            "${pkgs.emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
          installPhase =
            "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
        })
        epkgs.fringe-helper # for revive origami
      ];
      override = (self: super:
        {
          fuzzy-matcher = pkgs.emacs.pkgs.callPackage ./fuzzy-matcher.nix { };
          origami = super.origami.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = "elp-revive";
              repo = "origami.el";
              rev = "7b53c4c993d499b39d75a20326f52b63437bfa20";
              sha256 = "sha256-b58mv0wR/1lmuQtMwoP/AwhETJi2giCJYGUJxxL+3vc=";
            };
          });
        } // lib.genAttrs [
          "lsp-mode"
          "lsp-treemacs"
          "lsp-ui"
          "lsp-origami"
          "dap-mode"
          "lsp-java"
          "lsp-metals"
          "lsp-haskell"
        ] (name:
          super.${name}.overrideAttrs (super: { LSP_USE_PLISTS = true; })));
    });
  };

  home-manager = let
    home-config = {
      home.file.".config/emacs/init.el" = {
        source = ./init.el;
        onChange = ''
          rm -fv ~/.config/emacs/init.elc
          emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-byte-compile ~/.config/emacs/init.el
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
