{
  lib,
  pkgs,
  ...
}:

{
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

  systemd.user.services.sway-dbus-integration = {
    description = "sway-dbus-integration";
    wantedBy = [ "graphical.target" ];
    wants = [ "graphical.target" ];
    after = [ "graphical.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.hrdl-utils}/bin/sway_dbus_integration.py";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  systemd.user.services.pinenote-service-sway = {
    description = "pinenote-service";
    wantedBy = [ "graphical.target" ];
    wants = [ "graphical.target" ];
    after = [ "graphical.target" ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.pinenote-service}/bin/pinenote-service --sway";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  nixpkgs.overlays = [
    (_self: super: {
      pinenote-service = super.callPackage ./packages/pinenote-service.nix { };
    })
  ];

  home-manager.users.kurnevsky = {
    xdg.configFile."waybar/style.css".source = ./waybar/style.css;
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
        output."*".bg = "#FFFFFF solid_color";
        output."*".scale = "1";
        input."0:0:cyttsp5".map_to_output = "DPI-1";
        input."11551:149:w9013_2D1F:0095_Stylus".map_to_output = "DPI-1";
        bars = [ { command = "${lib.getExe pkgs.waybar}"; } ];
      };
    };
    programs = {
      waybar = {
        enable = true;
        settings.mainBar = import ./waybar.nix;
      };
    };
  };
}
