{ pkgs, lib, emacs, ... }:

pkgs.emacsWithPackagesFromUsePackage {
  config = ./init.el;
  package = emacs;
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
        "${emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
      installPhase =
        "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
    })
    (emacs.pkgs.callPackage ./fuzzy-matcher.nix { })
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
      treemacs = withDependency super.doom-modeline super.treemacs;
      origami = withDependency super.fringe-helper (super.origami.overrideAttrs
        (old: {
          src = pkgs.fetchFromGitHub {
            owner = "elp-revive";
            repo = "origami.el";
            rev = "a8300d79f8ba7429f656ea81ae74dd9ec7f9c894";
            sha256 = "sha256-Ysb1PTTvaM6Jig8JidMeNynnXyiG/YQ14ZRVqxGWyAU=";
          };
        }));
      scala-ts-mode = super.melpaBuild rec {
        pname = "scala-ts-mode";
        version = "1";
        commit = "e933ecd72d58e4dc02b81c8e2ed23da2f1948358";

        src = pkgs.fetchFromGitHub {
          owner = "KaranAhlawat";
          repo = pname;
          rev = commit;
          hash = "sha256-FdNWen0wjaY1c0O6O1iY018HZD/rYzxd0llhteqq8d0=";
        };

        recipe = pkgs.writeText "recipe" ''
          (scala-ts-mode :fetcher github
                         :repo "KaranAhlawat/scala-ts-mode"
                         :files ("scala-ts-mode.el"))
        '';
      };
    } // lib.genAttrs [
      "lsp-mode"
      "lsp-treemacs"
      "lsp-ui"
      "lsp-origami"
      "dap-mode"
      "lsp-java"
      "lsp-metals"
      "lsp-haskell"
    ] (name: super.${name}.overrideAttrs (super: { LSP_USE_PLISTS = true; })));
}
