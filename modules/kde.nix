{ config, pkgs, ... }:

{
  imports = [
    ./kwallet-secrets.nix
  ];

  services = {
    desktopManager.plasma6.enable = true;
    displayManager = {
      defaultSession = "plasma";
      sddm = {
        enable = true;
        wayland = {
          enable = true;
          compositor = "kwin";
        };
        autoNumlock = true;
        settings.Users.HideUsers = "ww";
      };
    };
  };

  environment.systemPackages =
    let
      command-desktop =
        name: command:
        let
          script = pkgs.writeShellScriptBin "${name}.sh" command;
        in
        pkgs.writeTextFile {
          name = "${name}.desktop";
          text = ''
            [Desktop Entry]
            Exec=${script}/bin/${name}.sh
            Name=${name}
            NoDisplay=true
            StartupNotify=false
            Type=Application
            X-KDE-GlobalAccel-CommandShortcut=true
          '';
          destination = "/share/applications/${name}.desktop";
        };
      brightness =
        name: awk:
        command-desktop name ''
          ${pkgs.dbus}/bin/dbus-send \
            --session \
            --type=method_call \
            --print-reply \
            --dest=org.kde.Solid.PowerManagement \
            /org/kde/Solid/PowerManagement/Actions/BrightnessControl \
            org.kde.Solid.PowerManagement.Actions.BrightnessControl.setBrightness \
            int32:$(
              ${pkgs.dbus}/bin/dbus-send \
                --session \
                --type=method_call \
                --print-reply=literal \
                --dest=org.kde.Solid.PowerManagement \
                /org/kde/Solid/PowerManagement/Actions/BrightnessControl \
                org.kde.Solid.PowerManagement.Actions.BrightnessControl.brightnessMax | \
                  ${pkgs.gawk}/bin/awk '${awk}'
            )
        '';
      brightness-min = brightness "brightness-min" "{print int($NF/10)}";
      brightness-max = brightness "brightness-max" "{print $NF}";
      translate-google = command-desktop "translate-google" ''
        ${pkgs.wl-clipboard}/bin/wl-paste --primary | \
          ${pkgs.translate-shell}/bin/trans -brief -t russian | \
          ${pkgs.findutils}/bin/xargs -0 notify-send 'google'
      '';
    in
    with pkgs;
    with kdePackages;
    with sandboxed;
    with kdePackages;
    [
      ark
      kcalc
      krfb
      krdc
      okular
      # TODO: broken
      # kamoso
      wl-clipboard
      plasma-pass
      # shortcuts
      brightness-min
      brightness-max
      translate-google
    ];

  xdg.portal = {
    enable = true;
    # Add the GTK portal which seems to be always needed for GTK applications
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };

  programs.kdeconnect.enable = true;

  # Things that still have to be configured manually:
  # - Theme: it could be configured with lookandfeeltool or https://github.com/maldoinc/plasma-theme-switcher but at the moment it doesn't work
  # - Touchpad: its configs are bound to exact touchpad model
  # - Entire dock panel

  home-manager.users =
    let
      home =
        { lib, ... }:
        let
          toValue =
            v:
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
              Effect-overview.GridBorderActivate = 5;
              Plugins.invertEnabled = true;
              TabBox.LayoutName = "thumbnails";
              Windows = {
                FocusPolicy = "FocusFollowsMouse";
                NextFocusPrefersMouse = true;
                Placement = "Maximizing";
                RollOverDesktops = false;
              };
              NightColor = {
                Active = true;
                EveningBeginFixed = 2200;
                Mode = "Times";
                NightTemperature = 5000;
                TransitionTime = 10;
              };
            };
            kdeglobals.KDE.SingleClick = false;
            kglobalshortcutsrc = {
              ksmserver = {
                "Lock Session" = "Meta+Esc	Screensaver,Meta+L	Screensaver,Lock Session";
                "Log Out" = "Meta+Shift+Esc,Ctrl+Alt+Del,Log Out";
              };
              kwin = {
                Invert = "Meta+Shift+I,Meta+Ctrl+I,Toggle Invert Effect";
                InvertWindow = "Meta+I,Meta+Ctrl+U,Toggle Invert Effect on Window";
                "Kill Window" = "Meta+Shift+C,Meta+Ctrl+Esc,Kill Window";
                "MoveMouseToCenter" = "Meta+Shift+W,Meta+F6,Move Mouse to Center";
                "MoveMouseToFocus" = "Meta+W,Meta+F5,Move Mouse to Focus";
                "Show Desktop" = "none,Meta+D,Peek at Desktop";
                "Switch One Desktop Down" = "Meta+Down,Meta+Ctrl+Down,Switch One Desktop Down";
                "Switch One Desktop Up" = "Meta+Up,Meta+Ctrl+Up,Switch One Desktop Up";
                "Switch One Desktop to the Left" = "Meta+Left,Meta+Ctrl+Left,Switch One Desktop to the Left";
                "Switch One Desktop to the Right" = "Meta+Right,Meta+Ctrl+Right,Switch One Desktop to the Right";
                "Switch to Desktop 1" = "Meta+1,Ctrl+F1,Switch to Desktop 1";
                "Switch to Desktop 2" = "Meta+2,Ctrl+F2,Switch to Desktop 2";
                "Switch to Desktop 3" = "Meta+3,Ctrl+F3,Switch to Desktop 3";
                "Switch to Desktop 4" = "Meta+4,Ctrl+F4,Switch to Desktop 4";
                "Switch to Desktop 5" = "Meta+5,,Switch to Desktop 5";
                "Switch to Desktop 6" = "Meta+6,,Switch to Desktop 6";
                "Switch to Desktop 7" = "Meta+7,,Switch to Desktop 7";
                "Switch to Desktop 8" = "Meta+8,,Switch to Desktop 8";
                "Switch to Desktop 9" = "Meta+9,,Switch to Desktop 9";
                "Switch to Screen 0" = "Meta+A,,Switch to Screen 0";
                "Switch to Screen 1" = "Meta+S,,Switch to Screen 1";
                "Switch to Screen 2" = "Meta+D,,Switch to Screen 2";
                "Walk Through Windows" = "Meta+Tab,Alt+Tab,Walk Through Windows";
                "Walk Through Windows (Reverse)" = "Meta+Shift+Tab,Alt+Shift+Tab,Walk Through Windows (Reverse)";
                "Walk Through Windows of Current Application" =
                  "Meta+`,Alt+`,Walk Through Windows of Current Application";
                "Walk Through Windows of Current Application (Reverse)" =
                  "Meta+~,Alt+~,Walk Through Windows of Current Application (Reverse)";
                "Window Close" = "Meta+C,Alt+F4,Close Window";
                "Window Fullscreen" = "Meta+Shift+F,,Make Window Fullscreen";
                "Window Maximize" = "Meta+F,Meta+PgUp,Maximize Window";
                "Window Minimize" = "Meta+Alt+F,Meta+PgDown,Minimize Window";
                "Window On All Desktops" = "Meta+V,none,Keep Window on All Desktops";
                "Window One Desktop Down" = "Meta+Shift+Down,Meta+Ctrl+Shift+Down,Window One Desktop Down";
                "Window One Desktop Up" = "Meta+Shift+Up,Meta+Ctrl+Shift+Up,Window One Desktop Up";
                "Window One Desktop to the Left" =
                  "Meta+Shift+Left,Meta+Ctrl+Shift+Left,Window One Desktop to the Left";
                "Window One Desktop to the Right" =
                  "Meta+Shift+Right,Meta+Ctrl+Shift+Right,Window One Desktop to the Right";
                "Window Quick Tile Bottom" = "Meta+Ctrl+Down,Meta+Down,Quick Tile Window to the Bottom";
                "Window Quick Tile Left" = "Meta+Ctrl+Left,Meta+Left,Quick Tile Window to the Left";
                "Window Quick Tile Right" = "Meta+Ctrl+Right,Meta+Right,Quick Tile Window to the Right";
                "Window Quick Tile Top" = "Meta+Ctrl+Up,Meta+Up,Quick Tile Window to the Top";
                "Window to Desktop 1" = "Meta+!,,Window to Desktop 1";
                "Window to Desktop 2" = "Meta+@,,Window to Desktop 2";
                "Window to Desktop 3" = "Meta+#,,Window to Desktop 3";
                "Window to Desktop 4" = "Meta+$,,Window to Desktop 4";
                "Window to Desktop 5" = "Meta+%,,Window to Desktop 5";
                "Window to Desktop 6" = "Meta+^,,Window to Desktop 6";
                "Window to Desktop 7" = "Meta+&,,Window to Desktop 7";
                "Window to Desktop 8" = "Meta+*,,Window to Desktop 8";
                "Window to Desktop 9" = "Meta+(,,Window to Desktop 9";
                "Window to Next Screen" = "none,Meta+Shift+Right,Move Window to Next Screen";
                "Window to Previous Screen" = "none,Meta+Shift+Left,Move Window to Previous Screen";
                "Window to Screen 0" = "Meta+Shift+A,none,Window to Screen 0";
                "Window to Screen 1" = "Meta+Shift+S,none,Window to Screen 1";
                "Window to Screen 2" = "Meta+Shift+D,none,Window to Screen 2";
              };
              plasmashell = {
                "activate task manager entry 1" = "none,Meta+1,Activate Task Manager Entry 1";
                "activate task manager entry 2" = "none,Meta+2,Activate Task Manager Entry 2";
                "activate task manager entry 3" = "none,Meta+3,Activate Task Manager Entry 3";
                "activate task manager entry 4" = "none,Meta+4,Activate Task Manager Entry 4";
                "activate task manager entry 5" = "none,Meta+5,Activate Task Manager Entry 5";
                "activate task manager entry 6" = "none,Meta+6,Activate Task Manager Entry 6";
                "activate task manager entry 7" = "none,Meta+7,Activate Task Manager Entry 7";
                "activate task manager entry 8" = "none,Meta+8,Activate Task Manager Entry 8";
                "activate task manager entry 9" = "none,Meta+9,Activate Task Manager Entry 9";
                "next activity" = "none,Meta+Tab,Walk through activities";
                "previous activity" = "none,Meta+Shift+Tab,Walk through activities (Reverse)";
                "show-on-mouse-pos" = "none,Meta+V,Open Klipper at Mouse Position";
                "stop current activity" = "none,Meta+S,Stop Current Activity";
              };
              org_kde_powerdevil = {
                "Turn Off Screen" = "Meta+F6,none,Turn Off Screen";
                "Decrease Screen Brightness Small" =
                  "Ctrl+Monitor Brightness Down,Shift+Monitor Brightness Down,Decrease Screen Brightness by 1%";
                "Increase Screen Brightness Small" =
                  "Ctrl+Monitor Brightness Up,Shift+Monitor Brightness Up,Increase Screen Brightness by 1%";
              };
              services = {
                "org.kde.spectacle.desktop" = {
                  _launch = "Print";
                  ActiveWindowScreenShot = "Meta+Print";
                  FullScreenScreenShot = "Shift+Print";
                  RecordRegion = "none";
                  RecordScreen = "none";
                  RecordWindow = "none";
                  RectangularRegionScreenShot = "Meta+Shift+Print";
                  WindowUnderCursorScreenShot = "Meta+Ctrl+Print";
                  CurrentMonitorScreenShot = "none";
                  OpenWithoutScreenshot = "none";
                };
                "org.kde.plasma-systemmonitor.desktop"._launch = "none";
                "org.kde.krunner.desktop"._launch = "Meta+F2	Search";
                "org.kde.konsole.desktop"._launch = "Meta+R";
                "Alacritty.desktop"._launch = "Meta+Shift+R";
                "brightness-min.desktop"._launch = "Shift+Monitor Brightness Down";
                "brightness-max.desktop"._launch = "Shift+Monitor Brightness Up";
                "translate-google.desktop"._launch = "Meta+Shift+T";
              };
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
            ksmserverrc = {
              General.loginMode = "emptySession";
            };
            kcminputrc = {
              Keyboard.NumLock = 0;
            };
          };
          toLine =
            file: path: value:
            if builtins.isAttrs value then
              lib.mapAttrsToList (group: value: toLine file (path ++ [ group ]) value) value
            else
              "test -f ~/.config/'${file}' && ${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file ~/.config/'${file}' ${
                lib.concatMapStringsSep " " (x: "--group ${x}") (lib.lists.init path)
              } --key '${lib.lists.last path}' ${toValue value}";
          lines = lib.flatten (lib.mapAttrsToList (file: attrs: toLine file [ ] attrs) configs);
        in
        {
          home = {
            activation.kdeConfigs = lib.hm.dag.entryAfter [ "writeBoundary" ] (
              builtins.concatStringsSep "\n" lines
            );

            file.".local/share/konsole/base16.colorscheme".text =
              let
                s = config.scheme;
                c = n: "${s."${n}-rgb-r"},${s."${n}-rgb-g"},${s."${n}-rgb-b"}";
              in
              lib.generators.toINI { } {
                Background.Color = c "base00";
                BackgroundIntense.Color = c "base03";
                BackgroundFaint.Color = c "base10";
                Color0.Color = c "base01";
                Color0Intense.Color = c "base02";
                Color0Faint.Color = c "base01";
                Color1.Color = c "base08";
                Color1Intense.Color = c "base12";
                Color1Faint.Color = c "base08";
                Color2.Color = c "base0B";
                Color2Intense.Color = c "base14";
                Color2Faint.Color = c "base0B";
                Color3.Color = c "base09";
                Color3Intense.Color = c "base13";
                Color3Faint.Color = c "base09";
                Color4.Color = c "base0D";
                Color4Intense.Color = c "base16";
                Color4Faint.Color = c "base0D";
                Color5.Color = c "base0E";
                Color5Intense.Color = c "base17";
                Color5Faint.Color = c "base0E";
                Color6.Color = c "base0C";
                Color6Intense.Color = c "base15";
                Color6Faint.Color = c "base0C";
                Color7.Color = c "base06";
                Color7Intense.Color = c "base07";
                Color7Faint.Color = c "base06";
                Foreground.Color = c "base05";
                ForegroundIntense.Color = c "base07";
                ForegroundFaint.Color = c "base04";
                General = {
                  Description = "Base16 Theme";
                  Opacity = 1;
                  Wallpaper = "";
                };
              };
          };
        };
    in
    {
      kurnevsky = home;
      ww = home;
    };
}
