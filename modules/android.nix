{ config, lib, pkgs, emacs-overlay, ... }:

let
  patchedPkgs = lib.foldl (pkg: pkg.extend) pkgs (import ./overlays.nix).nixpkgs.overlays;
  emacsPkgs = patchedPkgs.extend emacs-overlay;
  emacsWithPackages = emacsPkgs.callPackage ./emacs/package.nix {
    emacs = emacsPkgs.emacs29-nox;
  };
in {
  nix = {
    substituters =
      [ "https://cachix.cachix.org" "https://nix-community.cachix.org" ];
    trustedPublicKeys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      # local keys
      "pc:NVXsu6EBpoB+a+W2ZTkpTWD9Vj5nJtjYsAfe8zTzDKE="
      "evo:aBUuIOLObUPUBWBIW0XGyMZW2Wor5z3ZNtaRynZ35UY="
      "digitalocean:jFnutzjWmSNY5q/frkSkijlCh8GfdNa1Mpm5Y0N15sQ="
    ];
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  networking.hosts = {
    "192.168.14.2" = [ "home" ];
    "192.168.14.3" = [ "work" ];
    "192.168.14.4" = [ "parents" ];
    "192.168.14.5" = [ "pc" ];
    "200:3b5c:7981:d112:f1e2:7b3f:218f:e7ea" = [ "home-ygg" ];
    "200:d196:9226:6955:4190:dc33:bac7:17c7" = [ "work-ygg" ];
    "201:613a:44c6:38ad:a0f2:d452:dd0a:94c7" = [ "parents-ygg" ];
    "200:6381:a789:fbbe:3411:2135:e3b:b4a9" = [ "digitalocean-ygg" ];
    "201:b5cd:a295:d34c:a75f:b369:6002:c676" = [ "pc-ygg" ];
  };

  environment.packages = with patchedPkgs; [
    git
    lsof
    mc
    nano
    openssh
    gnupg
    procps
    gnugrep
    gnused
    gnutar
    bzip2
    gzip
    xz
    zip
    unzip
    diffutils
    findutils
    utillinux
    which
    ripgrep
    pass
    emacsWithPackages
  ];

  terminal.font = "${
      patchedPkgs.nerdfonts.override { fonts = [ "Hack" ]; }
    }/share/fonts/truetype/NerdFonts/HackNerdFontMono-Regular.ttf";

  user.shell = "${patchedPkgs.zsh}/bin/zsh";

  build.activation.termux = ''
    mkdir -p ~/.termux/
    rm -rf ~/.termux/termux.properties
    cp ${config.build.installationDir}/${
      ./termux.properties
    } ~/.termux/termux.properties
  '';

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    config = args@{ pkgs, ... }:
      lib.mkMerge [
        {
          # Override default prompt as it doesn't work good in Midnigt Commander
          # This has to be done before common-home.nix
          programs.zsh.initExtra = ''
            PROMPT='[%n@%m %~]$ '
          '';
        }
        (import ./common-home.nix args)
        (import ./emacs/home.nix emacsWithPackages args)
        {
          services.gpg-agent = {
            enable = true;
            pinentryPackage = pkgs.pinentry-curses;
          };
          programs = {
            bash.initExtra = ''
              # Set cursor type to steady bar
              echo -e -n "\x1b[\x36 q"

              alias -- ls='ls --color=auto'
              alias -- grep='grep --color=auto'
            '';
            zsh = {
              syntaxHighlighting.enable = true;
              autosuggestions.enable = true;
              initExtra = ''
                setopt ${lib.concatStringsSep " " (import ./zsh-options.nix)}

                ${builtins.readFile ./interactive-init.zsh}

                export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=10

                alias -- calc='noglob zcalc -e'
                alias -- zmv='noglob zmv -W'c
                alias -- ls='ls --color=auto'
                alias -- grep='grep --color=auto'
              '';
            };
          };
        }
      ];
  };
}
