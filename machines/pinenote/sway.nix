{
  lib,
  pkgs,
  ...
}:

let
  pinenoteConfig = "${pkgs.pinenote.config-sway}/share/pinenote-config";
  greeterSwayConfig = pkgs.writeText "greetd-sway-config" ''
    output * bg #FFFFFF solid_color
    # GTK_THEME: the PNEink theme renders squeekboard keys white-on-white.
    exec "env GTK_THEME=Adwaita ${pkgs.squeekboard}/bin/squeekboard"
    exec "${pkgs.gtkgreet}/bin/gtkgreet -l; ${pkgs.sway}/bin/swaymsg exit"
  '';
in
{
  services.greetd = {
    enable = true;
    settings = {
      initial_session = {
        command = "sway";
        user = "kurnevsky";
      };
      default_session = {
        command = "${pkgs.sway}/bin/sway --config ${greeterSwayConfig}";
        user = "greeter";
      };
    };
  };

  # Sessions offered by gtkgreet.
  environment.etc."greetd/environments".text = ''
    sway
  '';

  systemd.user.services.pinenote-service-sway = {
    description = "pinenote-service";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    after = [ "graphical-session.target" ];
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
      # The search-result scrolled windows don't propagate their natural
      # height, so the menu window stays tiny and 64px icons get cropped.
      nwg-menu = super.nwg-menu.overrideAttrs (old: {
        postPatch =
          (old.postPatch or "")
          + ''
            sed -i -E 's/([a-zA-Z]+)\.SetPolicy\(gtk\.POLICY_NEVER, gtk\.POLICY_AUTOMATIC\)/&\n\t\1.SetPropagateNaturalHeight(true)\n\t\1.SetMinContentHeight(600)/' uicomponents.go
          '';
      });
      pinenote-service = super.callPackage ./packages/pinenote-service.nix { };
      pinenote = {
        toggle-menu = pkgs.callPackage ./packages/toggle-menu.nix { };
        launch-lisgd = pkgs.callPackage ./packages/launch-lisgd.nix { };
        sway-rotate = pkgs.callPackage ./packages/sway-rotate.nix { };
        sway-workspace = pkgs.callPackage ./packages/sway-workspace.nix { };
        config-sway = pkgs.callPackage ./packages/pinenote-config-sway.nix { };
        pneink = pkgs.callPackage ./packages/pneink.nix { };
        transparent-cursor-theme = pkgs.callPackage ./packages/transparent-cursor-theme.nix { };
      };
    })
  ];

  programs.dconf = {
    enable = true;
    profiles.user.databases = [
      {
        settings = {
          # Needed for squeekboard, in particular in the greeter session.
          "org/gnome/desktop/a11y/applications".screen-keyboard-enabled = true;
          "org/gnome/desktop/interface" = {
            cursor-theme = "Transparent";
            enable-animations = false;
            gtk-theme = "PNEink";
            icon-theme = "Papirus";
          };
        };
      }
    ];
  };

  environment.systemPackages = with pkgs; [
    alacritty
    papirus-icon-theme
    pinenote.pneink
    pinenote.transparent-cursor-theme
    nwg-menu
    networkmanagerapplet
    thunar
    xournalpp
    koreader
  ];

  gtk.iconCache.enable = true;

  home-manager.users.kurnevsky = {
    # Suspend on idle; save/restore the frontlight around sleep since the
    # driver doesn't do it itself.
    services.swayidle = {
      enable = true;
      timeouts = [
        {
          timeout = 600;
          # wtype resets the idle timer in case suspend fails, allowing
          # swayidle to try again.
          command = "${pkgs.systemd}/bin/systemctl suspend || ${pkgs.wtype}/bin/wtype -k Escape";
        }
      ];
      events = [
        {
          event = "before-sleep";
          command = toString (
            pkgs.writeShellScript "before-sleep.sh" ''
              ${pkgs.brightnessctl}/bin/brightnessctl --save --device backlight_cool set 0
              ${pkgs.brightnessctl}/bin/brightnessctl --save --device backlight_warm set 0
            ''
          );
        }
        {
          event = "after-resume";
          command = toString (
            pkgs.writeShellScript "after-resume.sh" ''
              ${pkgs.brightnessctl}/bin/brightnessctl --restore --device backlight_warm
              ${pkgs.brightnessctl}/bin/brightnessctl --restore --device backlight_cool
            ''
          );
        }
      ];
    };
    gtk = {
      enable = true;
      theme = {
        name = "PNEink";
        package = pkgs.pinenote.pneink;
      };
      iconTheme = {
        name = "Papirus";
        package = pkgs.papirus-icon-theme;
      };
      cursorTheme = {
        name = "Transparent";
        package = pkgs.pinenote.transparent-cursor-theme;
      };
      gtk3.extraConfig.gtk-enable-animations = false;
      gtk4.extraConfig.gtk-enable-animations = false;
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
        fonts = {
          names = [ "Noto Sans" ];
          size = 20.0;
        };
        window = {
          border = 2;
          titlebar = true;
        };
        floating = {
          border = 2;
          titlebar = true;
        };
        gaps = {
          inner = 5;
          outer = 0;
          smartGaps = true;
        };
        output."*".bg = "#FFFFFF solid_color";
        output."*".scale = "1";
        input."0:0:cyttsp5".map_to_output = "DPI-1";
        input."11551:149:w9013_2D1F:0095_Stylus".map_to_output = "DPI-1";
        bars = [ ];
      };
      extraConfig = ''
        # Property Name         Border  BG      Text
        client.focused          #ffffff #000000 #ffffff
        client.focused_inactive #000000 #ffffff #000000
        client.unfocused        #ffffff #ffffff #000000
        client.urgent           #000000 #ffffff #000000

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
        # GTK_THEME: the PNEink theme renders squeekboard keys white-on-white.
        exec --no-startup-id env GTK_THEME=Adwaita ${pkgs.squeekboard}/bin/squeekboard &

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
