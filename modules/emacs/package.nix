{
  pkgs,
  lib,
  emacs,
  inputs,
  ...
}:

let
  emacsWithPackages = pkgs.emacsWithPackagesFromUsePackage {
    config = ./init.el;
    package = emacs;
    alwaysEnsure = true;
    extraEmacsPackages = epkgs: [
      (pkgs.stdenv.mkDerivation {
        name = "hexrgb.el";
        src = inputs.hexrgb;
        buildPhase = "${emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
        installPhase = "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
      })
      (emacs.pkgs.callPackage ./fuzzy-matcher.nix { })
      epkgs.treesit-grammars.with-all-grammars
    ];
    override =
      let
        # https://github.com/NixOS/nixpkgs/blob/80c49cf3d7731406605c9ebe87dd4601a7181e63/pkgs/applications/editors/emacs/build-support/generic.nix#L60-L61
        withDependency =
          d: p:
          p.overrideAttrs (old: {
            propagatedBuildInputs = old.propagatedBuildInputs ++ [ d ];
            propagatedUserEnvPkgs = old.propagatedUserEnvPkgs ++ [ d ];
          });
        withoutDependency =
          d: p:
          p.overrideAttrs (old: {
            propagatedBuildInputs = lib.lists.remove d old.propagatedBuildInputs;
            propagatedUserEnvPkgs = lib.lists.remove d old.propagatedUserEnvPkgs;
          });
      in
      _self: super:
      {
        # Doesn't properly depend on dash: https://github.com/dandavison/magit-delta/issues/30
        magit-delta = withDependency super.dash super.magit-delta;
        org-roam = withoutDependency super.org super.org-roam;
        org-ql = withoutDependency super.org super.org-ql;
        org-modern = withoutDependency super.org super.org-modern;
        org-super-agenda = withoutDependency super.org super.org-super-agenda;
        treemacs = withDependency super.doom-modeline super.treemacs;
        origami = withDependency super.fringe-helper (
          super.origami.overrideAttrs (old: {
            src = inputs."origami.el";

            postPatch =
              (old.postPatch or "")
              + ''
                substituteInPlace origami.el \
                  --replace-fail ",(face-attribute 'highlight :background)" 'nil'
              '';
          })
        );
        lean4-mode = super.melpaBuild rec {
          pname = "lean4-mode";
          version = "1.1.2";
          src = inputs.lean4-mode;

          buildInputs = with super; [
            dash
            flycheck
            lsp-mode
            magit
          ];

          recipe = pkgs.writeText "recipe" ''
            (lean4-mode :fetcher github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data"))
          '';
        };
      }
      //
        lib.genAttrs
          [
            "lsp-mode"
            "lsp-treemacs"
            "lsp-ui"
            "lsp-origami"
            "dap-mode"
            "lsp-java"
            "lsp-metals"
            "lsp-haskell"
          ]
          (
            name:
            super.${name}.overrideAttrs (_super: {
              env.LSP_USE_PLISTS = true;
            })
          );
  };
  with-direct = pkgs.callPackage ../with-direct.nix { };
in
pkgs.symlinkJoin {
  inherit (emacsWithPackages) name;
  paths = [
    (with-direct "${emacsWithPackages}/bin/emacs")
    (with-direct "${emacsWithPackages}/bin/emacsclient")
    emacsWithPackages
  ];
}
