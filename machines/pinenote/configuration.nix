{
  lib,
  pkgs,
  ...
}:

{
  boot.tmp.cleanOnBoot = true;

  i18n.supportedLocales = [
    "C.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "ru_RU.UTF-8/UTF-8"
  ];

  pinenote = {
    config.enable = true;
    pinenote-service.sway.enable = true;
    sway-dbus-integration.enable = true;
  };

  services.greetd = {
    enable = true;
    settings = rec {
      initial_session = {
        command = "sway";
        user = "kurnevsky";
      };
      default_session = initial_session;
    };
  };

  users.users.kurnevsky.extraGroups = [
    "dialout"
    "networkmanager"
    "video"
    "pipewire"
    "audio"
  ];

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager.enable = true;
    hostName = "pinenote";
  };

  hardware.bluetooth.enable = true;

  services = {
    journald.storage = "volatile";
    logind.extraConfig = ''
      HandlePowerKey=suspend
      HandlePowerKeyLongPress=poweroff
    '';
  };

  fileSystems = {
    "/" = {
      label = "nixos";
      fsType = "ext4";
    };
    "/home" = {
      label = "data";
      fsType = "ext4";
    };
  };

  system.stateVersion = "25.05";

  home-manager.users = {
    root.home.stateVersion = "25.05";
    kurnevsky = {
      wayland.windowManager.sway = {
        enable = true;
        systemd = {
          enable = true;
          variables = [ "--all" ];
        };
        wrapperFeatures.gtk = true;
        extraSessionCommands = ''
          # Session
          export XDG_SESSION_TYPE=wayland
          export XDG_SESSION_DESKTOP=sway
          export XDG_CURRENT_DESKTOP=sway

          # Wayland stuff
          export MOZ_ENABLE_WAYLAND=1
          export QT_QPA_PLATFORM=wayland
          export SDL_VIDEODRIVER=wayland
          export _JAVA_AWT_WM_NONREPARENTING=1
        '';
        config = rec {
          window = {
            border = 0;
            titlebar = false;
          };
          floating = window;
          output."*".bg = "#FFFFFF solid_color";
          output."*".scale = "2";
          input."0:0:cyttsp5".map_to_output = "DPI-1";
          input."11551:149:w9013_2D1F:0095_Stylus".map_to_output = "DPI-1";
          bars = [ { command = "${lib.getExe pkgs.waybar}"; } ];
          colors = {
            focused = {
              border = "#FFFFFF";
              background = "#000000";
              text = "#FFFFFF";
              indicator = "#FFFFFF";
              childBorder = "#FFFFFF";
            };
            focusedInactive = {
              border = "#000000";
              background = "#FFFFFF";
              text = "#000000";
              indicator = "#000000";
              childBorder = "#000000";
            };
            unfocused = {
              border = "#FFFFFF";
              background = "#FFFFFF";
              text = "#000000";
              indicator = "#FFFFFF";
              childBorder = "#FFFFFF";
            };
            urgent = {
              border = "#000000";
              background = "#FFFFFF";
              indicator = "#000000";
              childBorder = "#000000";
              text = "#000000";
            };
          };
        };
      };
      programs.git.enable = true;
      home.stateVersion = "25.05";
    };
  };
}
