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
      pinenote = {
        toggle-menu = pkgs.callPackage ./packages/toggle-menu.nix { };
        launch-lisgd = pkgs.callPackage ./packages/launch-lisgd.nix { };
        sway-rotate = pkgs.callPackage ./packages/sway-rotate.nix { };
        sway-workspace = pkgs.callPackage ./packages/sway-workspace.nix { };
      };
    })
  ];

  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    alacritty
    papirus-icon-theme
    nwg-menu
    networkmanagerapplet
    xfce.thunar
  ];

  gtk.iconCache.enable = true;

  home-manager.users.kurnevsky = {
    gtk = {
      enable = true;
      iconTheme = {
        name = "Papirus";
        package = pkgs.papirus-icon-theme;
      };
    };
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
        bars = [ ];
      };
      extraConfig = ''
        exec --no-startup-id ${pkgs.networkmanagerapplet}/bin/nm-applet --indicator &
        exec --no-startup-id ${pkgs.squeekboard}/bin/squeekboard &

        exec --no-startup-id ${pkgs.pinenote.sway-rotate}/bin/sway_rotate.sh rotnormal

        exec_always --no-startup-id ${pkgs.pinenote.launch-lisgd}/bin/launch_lisgd.sh
        exec_always --no-startup-id ${pkgs.util-linux}/bin/flock -n .sway_dbus_integration.lock ${pkgs.hrdl-utils}/bin/sway_dbus_integration.py
        exec rot8 --hooks ${pkgs.pinenote.launch-lisgd}/bin/launch_lisgd.sh --display DPI-1 --invert-x
      '';
    };
    programs = {
      waybar = {
        enable = true;
        systemd.enable = true;
        settings.mainBar = import ./waybar.nix { inherit pkgs; };
        style = builtins.readFile ./waybar/style.css;
      };
    };
    xdg.configFile."nwg-panel/menu-start.css".source = ./menu-start.css;
  };
}
