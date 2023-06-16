users:

{ pkgs, lib, ... }:

let
  emacs = (pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    package = pkgs.emacs-unstable-pgtk.overrideAttrs
      (old: { passthru = old.passthru // { treeSitter = true; }; });
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
      (pkgs.emacs.pkgs.callPackage ./fuzzy-matcher.nix { })
      epkgs.treesit-grammars.with-all-grammars
    ];
    override = let
      # https://github.com/NixOS/nixpkgs/blob/e6e389917a8c778be636e67a67ec958f511cc55d/pkgs/build-support/emacs/generic.nix#L48-L50
      withDependency = d: p:
        p.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ d ];
          propagatedBuildInputs = old.propagatedBuildInputs ++ [ d ];
          propagatedUserEnvPkgs = old.propagatedUserEnvPkgs ++ [ d ];
        });
      withoutDependency = d: p:
        p.overrideAttrs (old: {
          buildInputs = lib.lists.remove d old.buildInputs;
          propagatedBuildInputs = lib.lists.remove d old.propagatedBuildInputs;
          propagatedUserEnvPkgs = lib.lists.remove d old.propagatedUserEnvPkgs;
        });
    in (self: super:
      {
        org-roam = withoutDependency super.org super.org-roam;
        org-ql = withoutDependency super.org super.org-ql;
        org-super-agenda = withoutDependency super.org super.org-super-agenda;
        origami = withDependency super.fringe-helper
          (super.origami.overrideAttrs (old: {
            src = pkgs.fetchFromGitHub {
              owner = "elp-revive";
              repo = "origami.el";
              rev = "7b53c4c993d499b39d75a20326f52b63437bfa20";
              sha256 = "sha256-b58mv0wR/1lmuQtMwoP/AwhETJi2giCJYGUJxxL+3vc=";
            };
          }));
      } // lib.genAttrs [
        "lsp-mode"
        "lsp-treemacs"
        "lsp-ui"
        "lsp-origami"
        "dap-mode"
        "lsp-java"
        "lsp-metals"
        "lsp-haskell"
      ]
      (name: super.${name}.overrideAttrs (super: { LSP_USE_PLISTS = true; })));
  });
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
