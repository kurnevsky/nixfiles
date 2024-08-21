{ pkgs, lib, emacs, ... }:

let
  emacsWithPackages = pkgs.emacsWithPackagesFromUsePackage {
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
    in _self: super:
    {
      org-roam = withoutDependency super.org super.org-roam;
      org-ql = withoutDependency super.org super.org-ql;
      org-super-agenda = withoutDependency super.org super.org-super-agenda;
      treemacs = withDependency super.doom-modeline super.treemacs;
      origami = withDependency super.fringe-helper (super.origami.overrideAttrs
        (_old: {
          src = pkgs.fetchFromGitHub {
            owner = "elp-revive";
            repo = "origami.el";
            rev = "a8300d79f8ba7429f656ea81ae74dd9ec7f9c894";
            sha256 = "sha256-Ysb1PTTvaM6Jig8JidMeNynnXyiG/YQ14ZRVqxGWyAU=";
          };
        }));
      lean4-mode = super.melpaBuild rec {
        pname = "lean4-mode";
        version = "0";
        commit = "da7b63d854d010d621e2c82a53d6ae2d94dd53b0";

        src = pkgs.fetchFromGitHub {
          owner = "leanprover-community";
          repo = pname;
          rev = commit;
          hash = "sha256-U6MJIcBZf1XrUnssipgEy0BhF4/zhKVWblUvNqKNQe0=";
        };

        recipe = pkgs.writeText "recipe" ''
          (lean4-mode :fetcher github
                      :repo "leanprover-community/lean4-mode"
                      :files ("*.el" "data"))
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
    ] (name: super.${name}.overrideAttrs (_super: { LSP_USE_PLISTS = true; }));
  };
  with-direct = pkgs.callPackage ../with-direct.nix { };
in pkgs.symlinkJoin {
  inherit (emacsWithPackages) name;
  paths = [
    (with-direct "${emacsWithPackages}/bin/emacs")
    (with-direct "${emacsWithPackages}/bin/emacsclient")
    emacsWithPackages
  ];
}
