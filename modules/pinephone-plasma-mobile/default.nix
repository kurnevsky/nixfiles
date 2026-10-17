# Plasma Mobile, set up the way postmarketOS does it: postmarketos-ui-plasma-mobile
# (with -extras, -extras-web and angelfish), postmarketos-base-ui-plasma and
# Alpine's plasma-mobile.
{
  lib,
  pkgs,
  ...
}:

let
  inherit (pkgs) kdePackages;

  plasma-mobile = kdePackages.plasma-mobile.overrideAttrs (old: {
    # plasma-mobile-envmanager writes this path to ~/.config/plasma-mobile/kwinrc
    # as the default input method.
    postPatch = (old.postPatch or "") + ''
      substituteInPlace envmanager/config.h \
        --replace-fail /usr/share/applications/org.kde.plasma.keyboard.desktop \
          /run/current-system/sw/share/applications/org.kde.plasma.keyboard.desktop
    '';
    # Default homescreen favourites from postmarketOS.
    postInstall = (old.postInstall or "") + ''
      install -Dm644 ${./org.kde.plasma.mobile.homescreen.folio.js} \
        $out/share/plasma/look-and-feel/org.kde.breeze.mobile/contents/plasmoidsetupscripts/org.kde.plasma.mobile.homescreen.folio.js
    '';
  });

  okular-mobile = kdePackages.okular.overrideAttrs (old: {
    pname = "okular-mobile";
    cmakeFlags = (old.cmakeFlags or [ ]) ++ [ "-DOKULAR_UI=mobile" ];
    buildInputs = (old.buildInputs or [ ]) ++ [ kdePackages.kirigami-addons ];
  });

  plasma-mobile-sounds = pkgs.callPackage ./packages/plasma-mobile-sounds.nix {
    inherit (kdePackages) extra-cmake-modules;
  };
in
{
  imports = [ ../pinephone-ui-common ];

  services = {
    desktopManager.plasma6.enable = true;
    # There is no real display manager, but this enables services.graphical-desktop
    # (hardware.graphics, fonts, xdg integration), like greetd does.
    displayManager.enable = true;

    pipewire = {
      enable = true;
      pulse.enable = true;
    };

    dbus.packages = [
      pkgs.callaudiod
      # flashlight and waydroid helpers
      plasma-mobile
    ];

    udev.extraRules = ''
      # Allow everyone to set brightness of flashlight (required for plasma-mobile flashlightplugin)
      SUBSYSTEMS=="leds", KERNEL=="*:flash|*:torch", RUN+="${pkgs.coreutils}/bin/chmod 666 %S%p/brightness"
    '';
  };

  programs.kdeconnect.enable = true;

  # Desktop applications that pmOS doesn't install.
  environment.plasma6.excludePackages = with kdePackages; [
    aurorae
    gwenview
    kate
    khelpcenter
    konsole
    krdp
    ktexteditor
    kwin-x11
    okular
  ];

  environment.systemPackages = with kdePackages; [
    # plasma-mobile dependencies
    plasma-mobile
    bluez-qt
    kpipewire
    kquickcharts
    plasma-activities
    plasma-keyboard
    plasma-nano
    plasma-settings

    # postmarketos-ui-plasma-mobile
    pkgs.callaudiod
    dolphin
    kclock
    krecorder
    kweather
    plasma-camera
    plasma-dialer
    plasma-mobile-sounds
    plasma-phonebook
    qmlkonsole
    spacebar
    angelfish

    # postmarketos-ui-plasma-mobile-extras
    alligator
    audiotube
    elisa
    itinerary
    kalgebra
    kalk
    kasts
    keysmith

    # postmarketos-ui-plasma-mobile-extras-web
    arianna
    koko
    neochat
    okular-mobile
    tokodon

    # postmarketos-base-ui-plasma
    kaccounts-providers
  ];

  environment.etc."xdg/kscreenlockerrc".text = ''
    [Daemon]
    LockOnStart=true
  '';

  # Most pure GTK3 apps use wayland by default, but some,
  # like Firefox, need the backend to be explicitly selected.
  environment.sessionVariables.GDK_BACKEND = "wayland";

  systemd = {
    services = {
      # This prevents nixos-rebuild from killing plasma-mobile by activating getty again
      "autovt@tty1".enable = false;

      # Upstream plasma-mobile.service, which postmarketOS uses instead of a display manager.
      plasma-mobile = {
        description = "Open-source user interface for phones, based on Plasma technologies";
        documentation = [ "https://invent.kde.org/plasma-mobile" ];
        aliases = [ "display-manager.service" ];
        wantedBy = [ "graphical.target" ];
        wants = [ "dbus.socket" ];
        conflicts = [ "getty@tty1.service" ];
        after = [
          "getty@tty1.service"
          "plymouth-quit-wait.service"
          "systemd-user-sessions.service"
          "dbus.socket"
          # Workaround for a race in the setup of the user session by PAM.
          "session-c1.scope"
        ];
        before = [ "graphical.target" ];
        onFailure = [ "getty@tty1.service" ];
        unitConfig.ConditionPathExists = "/dev/tty0";
        environment = {
          XDG_SEAT = "seat0";
          XDG_VTNR = "1";
        };
        serviceConfig = {
          ExecStart = "${plasma-mobile}/bin/startplasmamobile";
          TimeoutStartSec = 30;
          User = "kurnevsky";
          PAMName = "login";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = "5s";

          TTYPath = "/dev/tty1";
          TTYReset = true;
          TTYVHangup = true;
          TTYVTDisallocate = true;

          StandardInput = "tty-fail";
          StandardOutput = "journal";
          StandardError = "journal";

          UtmpIdentifier = "tty1";
          UtmpMode = "user";
        };
        # Don't kill the session when using nixos-rebuild
        restartIfChanged = false;
      };
    };

    defaultUnit = "graphical.target";
  };

  home-manager.users.kurnevsky =
    { lib, ... }:
    {
      # Empty-password wallet, so that kwallet never asks for a password that
      # can't be typed with the on-screen keyboard.
      # See https://bugs.kde.org/show_bug.cgi?id=435183
      home.activation.kdewallet = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        if [ ! -e "$HOME/.local/share/kwalletd/kdewallet.kwl" ]; then
          run install -Dm600 -t "$HOME/.local/share/kwalletd" ${./kdewallet.kwl} ${./kdewallet.salt}
        fi
      '';
    };
}
