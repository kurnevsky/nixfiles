# Phosh, set up the way postmarketOS does it: postmarketos-ui-phosh,
# postmarketos-base-ui-gnome and postmarketos-base-ui-gnome-mobile.
{
  config,
  lib,
  pkgs,
  ...
}:

let
  phrog = pkgs.callPackage ./packages/phrog.nix { };
  cellbroadcastd = pkgs.callPackage ./packages/cellbroadcastd.nix { };
  lpa-gtk = pkgs.callPackage ./packages/lpa-gtk.nix { };
  phosh-tour = pkgs.callPackage ./packages/phosh-tour.nix { };
  phosh-wallpapers = pkgs.callPackage ./packages/phosh-wallpapers.nix { };
  postmarketos-tweaks = pkgs.callPackage ./packages/postmarketos-tweaks.nix { };

  phosh-mobile-settings = pkgs.phosh-mobile-settings.overrideAttrs (old: {
    mesonFlags = (old.mesonFlags or [ ]) ++ [
      (lib.mesonOption "tweaks-data-dir" "/run/current-system/sw/share/postmarketos-tweaks")
    ];
  });

  # pmOS's gschema overrides, compiled the same way as the GNOME module does it.
  gsettingsOverrides = pkgs.runCommand "pmos-gsettings-overrides" { preferLocalBuild = true; } ''
    schema_dir="$out/share/gsettings-schemas/pmos-gsettings-overrides/glib-2.0/schemas"
    mkdir -p "$schema_dir"

    for dir in ${
      lib.concatMapStringsSep " " pkgs.glib.getSchemaPath [
        pkgs.gsettings-desktop-schemas
        pkgs.gnome-settings-daemon
        pkgs.gnome-shell
        pkgs.gnome-software
        pkgs.phosh
        phosh-mobile-settings
        phrog
      ]
    }; do
      for f in "$dir"/*.xml "$dir"/*.gschema.override; do
        if [ -e "$f" ]; then
          cp -f "$f" "$schema_dir"
        fi
      done
    done
    cp ${./gschema-overrides}/*.gschema.override "$schema_dir"

    ${pkgs.glib.dev}/bin/glib-compile-schemas "$schema_dir"
  '';

  # Based on https://github.com/NixOS/nixpkgs/pull/558022
  phrogShell = pkgs.writeShellScript "phrog-shell" ''
    ${lib.getExe pkgs.stevia} &
    exec ${lib.getExe phrog}
  '';

  phrogSession = pkgs.writeShellScript "phrog-greetd-session" ''
    # Need to set XDG_CURRENT_DESKTOP now, otherwise the desktop files for the
    # on-screen keyboard with OnlyShowIn=Phosh are ignored.
    export XDG_CURRENT_DESKTOP=Phrog:Phosh:GNOME

    # So that phrog can find the sessions.
    export XDG_DATA_DIRS=${config.services.displayManager.sessionData.desktops}/share:/run/current-system/sw/share

    # greetd swallows all output from the greeters it spawns.
    exec > >(${config.systemd.package}/bin/systemd-cat --identifier=phrog) 2>&1

    # phoc needs a dbus session bus present or it fails to initialize.
    exec ${pkgs.dbus}/bin/dbus-run-session -- \
      ${lib.getExe pkgs.phoc} -S -C /etc/phosh/phoc.ini -E ${phrogShell}
  '';
in
{
  imports = [ ../pinephone-ui-common ];

  # The phosh module enables gnome core-os-services which defaults
  # i18n.inputMethod to ibus, and GTK_IM_MODULE=ibus prevents the on-screen
  # keyboard from auto-activating in GTK apps.
  # See https://github.com/NixOS/nixpkgs/issues/538381
  i18n.inputMethod.enable = false;

  services = {
    desktopManager.phosh = {
      enable = true;
      user = "kurnevsky";
      group = "users";
    };

    # pmOS starts phosh from greetd with the phrog greeter.
    greetd = {
      enable = true;
      settings.default_session.command = "${phrogSession}";
    };

    # phrog's user list comes from org.freedesktop.Accounts.
    accounts-daemon.enable = true;

    fprintd.enable = true;

    gnome = {
      gcr-ssh-agent.enable = true;
      gnome-keyring.enable = true;
      gnome-user-share.enable = true;
      rygel.enable = true;
    };

    gvfs.enable = true;

    udev.extraRules = ''
      # prevent from getting woken up by volume up / down in Phosh / Gnome Mobile
      SUBSYSTEM=="input", KERNEL=="event*", ENV{GM_WAKEUP_KEY_114}="0", ENV{GM_WAKEUP_KEY_115}="0"
    '';
  };

  # Started by phrog instead.
  systemd.services.phosh.enable = false;

  # phrog persists its last-user and last-session settings through dconf,
  # which needs a writable home.
  users.users.greeter.home = "/run/phrog";
  systemd.tmpfiles.settings."10-phrog"."/run/phrog".d = {
    user = "greeter";
    group = "greeter";
    mode = "0700";
  };

  programs = {
    calls.enable = true;
    xwayland.enable = true;
  };

  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-wlr ];

  xdg.mime.defaultApplications =
    lib.genAttrs [
      "x-scheme-handler/http"
      "x-scheme-handler/https"
      "text/html"
      "application/x-extension-htm"
      "application/x-extension-html"
      "application/x-extension-shtml"
      "application/xhtml+xml"
      "application/x-extension-xhtml"
      "application/x-extension-xht"
    ] (_: "firefox.desktop")
    // lib.genAttrs [
      "image/bmp"
      "image/gif"
      "image/jpeg"
      "image/jpg"
      "image/pjpeg"
      "image/png"
      "image/tiff"
      "image/x-bmp"
      "image/x-gray"
      "image/x-icb"
      "image/x-ico"
      "image/x-png"
      "image/x-portable-anymap"
      "image/x-portable-bitmap"
      "image/x-portable-graymap"
      "image/x-portable-pixmap"
      "image/x-xbitmap"
      "image/x-xpixmap"
      "image/x-pcx"
      "image/svg+xml"
      "image/svg+xml-compressed"
      "image/vnd.wap.wbmp"
      "image/x-icns"
    ] (_: "org.gnome.Loupe.desktop")
    // {
      "x-scheme-handler/tel" = "org.gnome.Calls.desktop";
      "x-scheme-handler/sms" = "sm.puri.Chatty.desktop";
    };

  environment = {
    sessionVariables = {
      GST_PLUGIN_SYSTEM_PATH_1_0 = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" (
        with pkgs.gst_all_1;
        [
          gst-plugins-base
          gst-plugins-good
          gst-plugins-bad
          gst-plugins-rs
          gst-libav
        ]
      );
      NIX_GSETTINGS_OVERRIDES_DIR = "${gsettingsOverrides}/share/gsettings-schemas/pmos-gsettings-overrides/glib-2.0/schemas";

      # postmarketos-base-ui-qt-tweaks
      QT_QUICK_CONTROLS_MOBILE = "true";
      # postmarketos-base-ui-qt-wayland
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      QT_QPA_PLATFORM = "wayland";
    };

    systemPackages = with pkgs; [
      # postmarketos-ui-phosh
      cellbroadcastd
      phosh-wallpapers
      udiskie
      phosh-mobile-settings
      phosh-tour

      # postmarketos-base-ui-gnome
      glycin-loaders
      glycin-thumbnailer
      gnome-backgrounds
      gnome-bluetooth
      gst-thumbnailers
      xdg-user-dirs
      xdg-user-dirs-gtk
      gnome-software
      decibels
      gapless
      gnome-calculator
      gnome-calendar
      gnome-clocks
      gnome-console
      gnome-contacts
      gnome-maps
      gnome-text-editor
      gnome-weather
      loupe
      nautilus
      papers
      showtime
      snapshot

      # postmarketos-base-ui-gnome-mobile
      chatty
      lpa-gtk
      postmarketos-tweaks
    ];
  };

  systemd = {
    packages = [
      cellbroadcastd
      phosh-tour
    ];

    user.services = {
      cellbroadcastd = {
        wantedBy = [ "default.target" ];
        aliases = [ "dbus-org.freedesktop.cbd.service" ];
      };

      "mobi.phosh.PhoshTour-first-login".wantedBy = [ "mobi.phosh.Shell.target" ];

      # Notifications from udiskie are disabled because Phosh sends its own
      # notifications on drive connect/disconnect.
      udiskie = {
        description = "udisks2 frontend";
        wantedBy = [ "graphical-session.target" ];
        partOf = [ "graphical-session.target" ];
        after = [ "graphical-session.target" ];
        serviceConfig = {
          ExecStart = "${lib.getExe pkgs.udiskie} -N";
          Restart = "on-failure";
        };
      };
    };
  };

  home-manager.users.kurnevsky.dconf.settings."mobi/phosh/osk".ignore-hw-keyboards = true;
}
