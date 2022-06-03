{ lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    displayManager = {
      defaultSession = "plasmawayland";
      sddm = {
        enable = true;
        autoNumlock = true;
        settings.Users.HideUsers = "ww";
      };
    };
    desktopManager.plasma5.enable = true;
  };

  environment.systemPackages = with pkgs; [
    ark
    kcalc
    krfb
    krdc
  ];

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
    # Add the GTK portal which seems to be always needed for GTK applications
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk ];
  };

  programs.kdeconnect.enable = true;

  # Things that still have to be configured manually:
  # - Theme: it could be configured with lookandfeeltool or https://github.com/maldoinc/plasma-theme-switcher but at the moment it doesn't work
  # - Touchpad: its configs are bound to exact touchpad model
  # - Entire dock panel

  home-manager.users = let
    home = { lib, ... }:
      let
        toValue = v:
          if v == null then
            "--delete"
          else if builtins.isString v then
            "'" + v + "'"
          else if builtins.isBool v then
            "--type bool " + lib.boolToString v
          else if builtins.isInt v then
            builtins.toString v
          else
            builtins.abort ("Unknown value type: " ++ builtins.toString v);
        configs = {
          kwinrc = {
            Compositing = {
              GLCore = true;
              OpenGLIsUnsafe = false;
            };
            Desktops = {
              Id_1 = "0f58e871-15cd-41d9-b264-f32e8cf4efc0";
              Id_2 = "ae53fcaa-bdb2-4c4a-a2f8-56b856436d4f";
              Id_3 = "3f087407-bee1-4ffd-8406-93daa6d6c8ef";
              Id_4 = "173d1149-fb35-457f-b5ad-83565b76af17";
              Id_5 = "beac7049-1de6-4e72-867b-5d022ecdabb6";
              Id_6 = "51980c0a-2ca4-420c-89d6-9c44793fff9f";
              Id_7 = "e15b8d6e-ca11-4f48-ab7c-21bee08d196b";
              Id_8 = "9a84dcbd-836e-4fa0-a8b8-d13132fbd2dd";
              Id_9 = "430241ed-707e-442a-9836-84702bd2bde0";
              Number = 9;
              Rows = 3;
            };
            Effect-DesktopGrid.BorderActivate = 5;
            Effect-PresentWindows = {
              BorderActivate = 7;
              BorderActivateAll = 9;
              MiddleButtonWindow = 6;
            };
            Effect-Slide.Duration = 300;
            Plugins.invertEnabled = true;
            TabBox.LayoutName = "thumbnail_grid";
            Windows = {
              FocusPolicy = "FocusFollowsMouse";
              NextFocusPrefersMouse = true;
              Placement = "Maximizing";
              RollOverDesktops = false;
            };
          };
          kdeglobals.KDE.SingleClick = false;
          kglobalshortcutsrc = {
            ksmserver = {
              "Lock Session" =
                "Meta+Esc	Screensaver,Meta+L	Ctrl+Alt+L	Screensaver,Lock Session";
              "Log Out" = "Meta+Shift+Esc,Ctrl+Alt+Del,Log Out";
            };
            kwin = {
              Invert = "Meta+Shift+I,Meta+Ctrl+I,Toggle Invert Effect";
              InvertWindow =
                "Meta+I,Meta+Ctrl+U,Toggle Invert Effect on Window";
              "Kill Window" = "Meta+Shift+C,Ctrl+Alt+Esc,Kill Window";
              "MoveMouseToCenter" = "Meta+Shift+W,Meta+F6,Move Mouse to Center";
              "MoveMouseToFocus" = "Meta+W,Meta+F5,Move Mouse to Focus";
              "Show Desktop" = "none,Meta+D,Show Desktop";
              "Switch One Desktop Down" =
                "Meta+Down,Meta+Ctrl+Down,Switch One Desktop Down";
              "Switch One Desktop Up" =
                "Meta+Up,Meta+Ctrl+Up,Switch One Desktop Up";
              "Switch One Desktop to the Left" =
                "Meta+Left,Meta+Ctrl+Left,Switch One Desktop to the Left";
              "Switch One Desktop to the Right" =
                "Meta+Right,Meta+Ctrl+Right,Switch One Desktop to the Right";
              "Switch to Desktop 1" = "Meta+1,Ctrl+F1,Switch to Desktop 1";
              "Switch to Desktop 2" = "Meta+2,Ctrl+F2,Switch to Desktop 2";
              "Switch to Desktop 3" = "Meta+3,Ctrl+F3,Switch to Desktop 3";
              "Switch to Desktop 4" = "Meta+4,Ctrl+F4,Switch to Desktop 4";
              "Switch to Desktop 5" = "Meta+5,none,Switch to Desktop 5";
              "Switch to Desktop 6" = "Meta+6,none,Switch to Desktop 6";
              "Switch to Desktop 7" = "Meta+7,none,Switch to Desktop 7";
              "Switch to Desktop 8" = "Meta+8,none,Switch to Desktop 8";
              "Switch to Desktop 9" = "Meta+9,none,Switch to Desktop 9";
              "Switch to Screen 0" = "Meta+A,none,Switch to Screen 0";
              "Switch to Screen 1" = "Meta+S,none,Switch to Screen 1";
              "Switch to Screen 2" = "Meta+D,none,Switch to Screen 2";
              "Walk Through Windows" = "Meta+Tab,Alt+Tab,Walk Through Windows";
              "Walk Through Windows (Reverse)" =
                "Meta+Shift+Backtab,Alt+Shift+Backtab,Walk Through Windows (Reverse)";
              "Walk Through Windows of Current Application" =
                "Meta+`,Alt+`,Walk Through Windows of Current Application";
              "Walk Through Windows of Current Application (Reverse)" =
                "Meta+~,Alt+~,Walk Through Windows of Current Application (Reverse)";
              "Window Close" = "Meta+C,Alt+F4,Close Window";
              "Window Fullscreen" = "Meta+Shift+F,none,Make Window Fullscreen";
              "Window Maximize" = "Meta+F,Meta+PgUp,Maximize Window";
              "Window Minimize" = "Meta+Alt+F,Meta+PgDown,Minimize Window";
              "Window On All Desktops" =
                "Meta+V,none,Keep Window on All Desktops";
              "Window One Desktop Down" =
                "Meta+Shift+Down,Meta+Ctrl+Shift+Down,Window One Desktop Down";
              "Window One Desktop Up" =
                "Meta+Shift+Up,Meta+Ctrl+Shift+Up,Window One Desktop Up";
              "Window One Desktop to the Left" =
                "Meta+Shift+Left,Meta+Ctrl+Shift+Left,Window One Desktop to the Left";
              "Window One Desktop to the Right" =
                "Meta+Shift+Right,Meta+Ctrl+Shift+Right,Window One Desktop to the Right";
              "Window Quick Tile Bottom" =
                "Meta+Ctrl+Down,Meta+Down,Quick Tile Window to the Bottom";
              "Window Quick Tile Left" =
                "Meta+Ctrl+Left,Meta+Left,Quick Tile Window to the Left";
              "Window Quick Tile Right" =
                "Meta+Ctrl+Right,Meta+Right,Quick Tile Window to the Right";
              "Window Quick Tile Top" =
                "Meta+Ctrl+Up,Meta+Up,Quick Tile Window to the Top";
              "Window to Desktop 1" = "Meta+!,none,Window to Desktop 1";
              "Window to Desktop 2" = "Meta+@,none,Window to Desktop 2";
              "Window to Desktop 3" = "Meta+#,none,Window to Desktop 3";
              "Window to Desktop 4" = "Meta+$,none,Window to Desktop 4";
              "Window to Desktop 5" = "Meta+%,none,Window to Desktop 5";
              "Window to Desktop 6" = "Meta+^,none,Window to Desktop 6";
              "Window to Desktop 7" = "Meta+&,none,Window to Desktop 7";
              "Window to Desktop 8" = "Meta+*,none,Window to Desktop 8";
              "Window to Desktop 9" = "Meta+(,none,Window to Desktop 9";
              "Window to Next Screen" =
                "none,Meta+Shift+Right,Window to Next Screen";
              "Window to Previous Screen" =
                "none,Meta+Shift+Left,Window to Previous Screen";
              "Window to Screen 0" = "Meta+Shift+A,none,Window to Screen 0";
              "Window to Screen 1" = "Meta+Shift+S,none,Window to Screen 1";
              "Window to Screen 2" = "Meta+Shift+D,none,Window to Screen 2";
            };
            plasmashell = {
              "activate task manager entry 1" =
                "none,Meta+1,Activate Task Manager Entry 1";
              "activate task manager entry 2" =
                "none,Meta+2,Activate Task Manager Entry 2";
              "activate task manager entry 3" =
                "none,Meta+3,Activate Task Manager Entry 3";
              "activate task manager entry 4" =
                "none,Meta+4,Activate Task Manager Entry 4";
              "activate task manager entry 5" =
                "none,Meta+5,Activate Task Manager Entry 5";
              "activate task manager entry 6" =
                "none,Meta+6,Activate Task Manager Entry 6";
              "activate task manager entry 7" =
                "none,Meta+7,Activate Task Manager Entry 7";
              "activate task manager entry 8" =
                "none,Meta+8,Activate Task Manager Entry 8";
              "activate task manager entry 9" =
                "none,Meta+9,Activate Task Manager Entry 9";
              "next activity" = "none,Meta+Tab,Walk through activities";
              "previous activity" =
                "none,Meta+Shift+Tab,Walk through activities (Reverse)";
              "show-on-mouse-pos" =
                "none,Meta+V,Open Klipper at Mouse Position";
              "stop current activity" = "none,Meta+S,Stop Current Activity";
            };
            org_kde_powerdevil = {
              "Turn Off Screen" = "Meta+F6,none,Turn Off Screen";
            };
            "org.kde.krunner.desktop" = {
              "_launch" = "Meta+F2	Search,Alt+Space	Alt+F2	Search,KRunner";
            };
            "org.kde.dolphin.desktop" = {
              "_launch" = "Meta+E,Meta+E,Dolphin";
            };
            "Alacritty.desktop" = { "_launch" = "Meta+R,,Alacritty"; };
          };
          kxkbrc = {
            Layout = {
              LayoutList = "us,ru";
              Options = "grp:caps_toggle,grp_led:caps";
              SwitchMode = "Global";
              Use = true;
            };
          };
          baloofilerc = {
            "Basic Settings".Indexing-Enabled = false;
            General."only basic indexing" = true;
          };
          plasma-localerc = {
            Formats = {
              LANG = "en_US.UTF-8";
              LC_MEASUREMENT = "ru_RU.UTF-8";
              useDetailed = true;
            };
          };
          ksmserverrc = { General.loginMode = "emptySession"; };
          kcminputrc = { Keyboard.NumLock = 0; };
          khotkeysrc = {
            Data.DataCount = 6;
            # First 3 are set by KDE by default
            Data_4 = {
              Comment = "Set brightness to 10%";
              Enabled = true;
              Name = "Brightness 10%";
              Type = "SIMPLE_ACTION_DATA";
            };
            Data_4Actions.ActionsCount = 1;
            Data_4Actions0 = {
              CommandURL = ''
                dbus-send --session --type=method_call --print-reply --dest=org.kde.Solid.PowerManagement /org/kde/Solid/PowerManagement/Actions/BrightnessControl org.kde.Solid.PowerManagement.Actions.BrightnessControl.setBrightness int32:$(dbus-send --session --type=method_call --print-reply=literal --dest=org.kde.Solid.PowerManagement /org/kde/Solid/PowerManagement/Actions/BrightnessControl org.kde.Solid.PowerManagement.Actions.BrightnessControl.brightnessMax | awk "{print int(\$NF/10)}")'';
              Type = "COMMAND_URL";
            };
            Data_4Triggers = {
              Comment = "Simple_action";
              TriggersCount = 1;
            };
            Data_4Triggers0 = {
              Key = "Shift+Monitor Brightness Down";
              Type = "SHORTCUT";
              Uuid = "{bc4dfdf5-1244-42f9-989e-22a5f6d21f73}";
            };
            Data_5 = {
              Comment = "Set brightness to 100%";
              Enabled = true;
              Name = "Brightness 100%";
              Type = "SIMPLE_ACTION_DATA";
            };
            Data_5Actions.ActionsCount = 1;
            Data_5Actions0 = {
              CommandURL = ''
                dbus-send --session --type=method_call --print-reply --dest=org.kde.Solid.PowerManagement /org/kde/Solid/PowerManagement/Actions/BrightnessControl org.kde.Solid.PowerManagement.Actions.BrightnessControl.setBrightness int32:$(dbus-send --session --type=method_call --print-reply=literal --dest=org.kde.Solid.PowerManagement /org/kde/Solid/PowerManagement/Actions/BrightnessControl org.kde.Solid.PowerManagement.Actions.BrightnessControl.brightnessMax | awk "{print \$NF}")'';
              Type = "COMMAND_URL";
            };
            Data_5Triggers = {
              Comment = "Simple_action";
              TriggersCount = 1;
            };
            Data_5Triggers0 = {
              Key = "Shift+Monitor Brightness Up";
              Type = "SHORTCUT";
              Uuid = "{42547858-8c2f-4a2b-916e-2cd916b011ce}";
            };
            Data_6 = {
              Comment = "Switch User";
              Enabled = true;
              Name = "Switch User";
              Type = "SIMPLE_ACTION_DATA";
            };
            Data_6Actions.ActionsCount = 1;
            Data_6Actions0 = {
              CommandURL =
                "dbus-send --session --type=method_call --print-reply=literal --dest=org.kde.krunner /App org.kde.krunner.App.switchUser";
              Type = "COMMAND_URL";
            };
            Data_6Triggers = {
              Comment = "Simple_action";
              TriggersCount = 1;
            };
            Data_6Triggers0 = {
              Key = "Meta+Alt+Esc";
              Type = "SHORTCUT";
              Uuid = "{7c6b0e9c-46af-4839-bb1f-5a805d8b4b3c}";
            };
          };
        };
        lines = lib.flatten (lib.mapAttrsToList (file: groups:
          lib.mapAttrsToList (group: keys:
            lib.mapAttrsToList (key: value:
              "test -f ~/.config/'${file}' && ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file ~/.config/'${file}' --group '${group}' --key '${key}' ${
                toValue value
              }") keys) groups) configs);
      in {
        home.activation.kdeConfigs = lib.hm.dag.entryAfter [ "writeBoundary" ]
          (builtins.concatStringsSep "\n" lines);
      };
  in {
    kurnevsky = home;
    ww = home;
  };
}
