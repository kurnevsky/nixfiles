{
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = with pkgs; [
    PNEink
    gnome-console
    gnome-tweaks
    gnomeExtensions.user-themes
    (gnomeExtensions.maximized-by-default-reborn.overrideAttrs (old: {
      postPatch = ''
        substituteInPlace metadata.json \
          --replace-fail '"46"' '"48"'
      '';
    }))
    pinenote-gnome-extension
  ];

  services = {
    displayManager.gdm = {
      enable = true;
      settings.daemon = {
        AutomaticLoginEnable = true;
        AutomaticLogin = "kurnevsky";
      };
    };
    desktopManager.gnome.enable = true;
    gnome = {
      core-apps.enable = false;
      core-developer-tools.enable = false;
      games.enable = false;
    };
    dbus.packages = [ pkgs.pinenote-dbus-service ];
  };

  systemd.services.pinenote-dbus-service = {
    description = "pinenote-dbus-service";
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      Type = "simple";
      ExecStart = "${pkgs.pinenote-dbus-service}/bin/pinenote_dbus_service";
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  nixpkgs.overlays = [
    (_self: super: {
      pinenote-dbus-service = super.callPackage ./packages/pinenote-dbus-service.nix { };
      PNEink = super.callPackage ./packages/pneink.nix { };
      pinenote-gnome-extension = super.callPackage ./packages/pinenote-gnome-extension.nix { };
    })
  ];

  home-manager.users.kurnevsky.dconf.settings = {
    "org/gnome/desktop/a11y/applications".screen-keyboard-enabled = true;

    "org/gnome/desktop/input-sources" = {
      show-all-sources = true;
      sources = [
        (lib.gvariant.mkTuple [
          "xkb"
          "us"
        ])
        (lib.gvariant.mkTuple [
          "xkb"
          "ru"
        ])
      ];
    };

    "org/gnome/desktop/a11y/interface".high-contrast = true;

    "org/gnome/desktop/a11y/keyboard" = {
      mousekeys-enable = false;
      stickykeys-enable = true;
    };

    # [org/gnome/desktop/interface]
    # clock-show-date=false
    # clock-show-weekday=false
    # cursor-blink=false
    # cursor-size=14
    # cursor-theme='breeze_cursors'
    # enable-animations=false
    # font-antialiasing='grayscale'
    # font-hinting='slight'
    # font-name='Noto Sans 11'
    # gtk-theme='hicolor'
    # icon-theme='Adwaita'
    # show-battery-percentage=true
    # toolbar-style='text'
    # toolkit-accessibility=true

    "org/gnome/desktop/search-providers".disable-external = false;

    "org/gnome/desktop/session".idle-delay = "uint32 0";

    # [org/gnome/desktop/wm/preferences]
    # action-double-click-titlebar='menu'
    # button-layout='close,maximize:appmenu'
    # theme='HighContrast'
    # visual-bell=false

    "org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions = [
        "pnhelper@m-weigand.github.com"
        "user-theme@gnome-shell-extensions.gcampax.github.com"
        "maximized-by-default@brennoflavio.com.br"
      ];
      # favorite-apps=['org.gnome.Terminal.desktop', 'dev.tchx84.Portfolio.desktop', 'firefox-esr.desktop', 'koreader.desktop', 'com.github.xournalpp.xournalpp.desktop', 'pnhelp.desktop']
    };

    "org/gnome/shell/extensions/user-theme".name = "PNEink";

    "org/gnome/desktop/lockdown".disable-lock-screen = true;

    "org/gnome/mutter" = {
      auto-maximize = true;
      center-new-windows = true;
    };

    # [org/gnome/nautilus/preferences]
    # click-policy='single'
    # default-folder-viewer='icon-view'
    # search-filter-time-type='last_modified'
    # search-view='list-view'

    "org/gnome/desktop/peripherals/stylus/default-2d1f:0095" = {
      button-action = "right";
      eraser-pressure-curve = [
        0
        0
        100
        100
      ];
      pressure-curve = [
        0
        0
        100
        100
      ];
    };

    # [org/gnome/settings-daemon/peripherals/touchscreen]
    # orientation-lock=true

    "org/gnome/settings-daemon/plugins/power" = {
      sleep-inactive-ac-type = "nothing";
      sleep-inactive-battery-timeout = 1800;
    };
  };
}
