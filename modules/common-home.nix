{ lib, pkgs, ... }:

{
  home = {
    activation.checkLinkTargets = lib.mkForce (lib.hm.dag.entryAfter [ "writeBoundary" ] "true");
    file.".config/mc/ini".source = ./mc.ini;
  };
  programs = {
    bash = {
      enable = true;
      historySize = 25000;
      historyControl = [
        "erasedups"
        "ignoredups"
        "ignorespace"
      ];
      sessionVariables.PS1 = "[u@h W]$ ";
      initExtra = ''
        if [[ $TERM != "dumb" && -z "$MC_SID" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
          eval "$(${pkgs.starship}/bin/starship init bash)"
        fi
      '';
    };
    zsh = {
      enable = true;
      enableCompletion = false;
      history = {
        size = 20000;
        save = 25000;
        path = "$HOME/.histfile";
        expireDuplicatesFirst = true;
        share = false;
        ignoreDups = false;
      };
      initContent = ''
        if [[ $TERM != "dumb" && -z "$MC_SID" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
          eval "$(${pkgs.starship}/bin/starship init zsh)"
        fi
      '';
    };
    starship = {
      enable = true;
      # Enabled manually since there is no way to disable it for mc
      enableBashIntegration = false;
      enableZshIntegration = false;
      enableFishIntegration = false;
      settings = {
        add_newline = false;
        format = "$username$hostname$localip$shlvl$directory$character";
        right_format = "$git_branch$git_commit$git_state$git_metrics$git_status$hg_branch$nix_shell$cmd_duration";
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
        directory = {
          truncation_length = 0;
          truncate_to_repo = false;
        };
      };
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      historyLimit = 10000;
      extraConfig = builtins.readFile ./tmux.conf;
    };
    direnv = {
      enable = true;
      config.global.strict_env = true;
    };
    ssh = {
      enable = true;
      compression = true;
      hashKnownHosts = true;
    };
    git = {
      userName = "Evgeny Kurnevsky";
      userEmail = "kurnevsky@gmail.com";
      signing = {
        key = null;
        signByDefault = true;
      };
      aliases = {
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
      };
      delta.enable = true;
      lfs.enable = true;
      extraConfig = {
        init.defaultBranch = "master";
        push.default = "simple";
        merge.conflictstyle = "diff3";
        pull.ff = "only";
        safe.directory = "/etc/nixos";
        github.user = "kurnevsky";
        gitlab.user = "kurnevsky";
        gitlab."gitlab.evolution.com/api/v4".user = "ykurneuski";
      };
    };
    nix-index.enable = true;
  };
  services.gpg-agent =
    let
      ttl = 180 * 24 * 60 * 60;
    in
    {
      enableSshSupport = true;
      sshKeys = [ "53D3B2AAF43FA184A31ACEC71295A713D5B9A123" ];
      defaultCacheTtl = ttl;
      maxCacheTtl = ttl;
      defaultCacheTtlSsh = ttl;
      maxCacheTtlSsh = ttl;
      extraConfig = "allow-loopback-pinentry";
    };
}
