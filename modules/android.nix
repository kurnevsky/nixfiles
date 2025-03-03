{
  config,
  lib,
  pkgs,
  emacs-overlay,
  inputs,
  ...
}:

let
  applyOverlays = pkgs: lib.foldl (pkg: pkg.extend) pkgs (import ./overlays.nix).nixpkgs.overlays;
  patchedPkgs = applyOverlays pkgs;
  emacsPkgs = patchedPkgs.extend emacs-overlay;
  emacsWithPackages = emacsPkgs.callPackage ./emacs/package.nix {
    emacs = emacsPkgs.emacs30-nox;
    inherit inputs;
  };
in
{
  nix = {
    substituters = [
      "https://cachix.cachix.org"
      "https://nix-community.cachix.org"
    ];
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

  networking.hosts = import ./hosts.nix;

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
    let
      pkgs86 = import patchedPkgs.path { system = "x86_64-linux"; };
      patchedPkgs86 = applyOverlays pkgs86;
      iosevka-custom = patchedPkgs86.callPackage ./iosevka.nix { };
      iosevka-term = iosevka-custom "Term" false;
    in
    patchedPkgs86.callPackage ./nerd-font-patch.nix { } iosevka-term
  }/share/fonts/truetype/IosevkaTerm-Regular.ttf";

  user.shell = "${patchedPkgs.zsh}/bin/zsh";

  build.activation.termux = ''
    mkdir -p ~/.termux/
    rm -rf ~/.termux/termux.properties
    cp ${config.build.installationDir}/${./termux.properties} ~/.termux/termux.properties
  '';

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    config =
      args@{ pkgs, ... }:
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
              autosuggestion.enable = true;
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
