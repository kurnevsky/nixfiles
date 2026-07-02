{
  lib,
  pkgs,
  ...
}:

let
  pinenoteConfig = "${pkgs.pinenote.config-sway}/share/pinenote-config";
in
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
        config-sway = pkgs.callPackage ./packages/pinenote-config-sway.nix { };
      };
    })
  ];

  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    alacritty
    papirus-icon-theme
    nwg-menu
    networkmanagerapplet
    thunar
    xournalpp
    koreader
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
        set $menu ${pkgs.pinenote.toggle-menu}/bin/toggle_menu.sh
        set $toggle_osk ${pinenoteConfig}/sway/scripts/toggle_squeekboard.sh
        set $gestures_service ${pkgs.pinenote.launch-lisgd}/bin/launch_lisgd.sh
        set $pn_ebcmark ${pinenoteConfig}/sway/scripts/ebcmark.sh

        bar {
            position top
            swaybar_command ${pinenoteConfig}/sway/scripts/start_waybar.sh
            pango_markup enable
        }

        bar bar-ctrl {
            swaybar_command ${pkgs.coreutils}/bin/true
            mode hide
            pango_markup enable
        }

        exec_always --no-startup-id $gestures_service
        exec --no-startup-id ${pinenoteConfig}/sway/scripts/sway_rotate.sh start

        exec --no-startup-id ${pkgs.networkmanagerapplet}/bin/nm-applet --indicator &
        exec --no-startup-id ${pkgs.squeekboard}/bin/squeekboard &

        for_window [app_id="mpv"] exec $pn_ebcmark set "Y1|D" silent
        for_window [app_id="KOReader"] exec $pn_ebcmark set "Y4" silent
        for_window [app_id="Alacritty"] exec $pn_ebcmark set "Y2|R" silent
        for_window [app_id="com.github.xournalpp.xournalpp"] exec $pn_ebcmark set "Y4|R" silent
        for_window [app_id="firefox"] exec $pn_ebcmark set "Y4|R" silent
        for_window [app_id="org.qutebrowser.qutebrowser"] exec $pn_ebcmark set "Y4|R" silent
        for_window [app_id="mepo"] exec $pn_ebcmark set "Y4|R" silent
        for_window [app_id="imv"] exec $pn_ebcmark set "Y4|R" silent
      '';
    };
    xdg.configFile."nwg-panel/menu-start.css".source = ./menu-start.css;
  };
}
