{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    numlockx
    wmctrl
    xcalib
    xcb-client-id
    xdotool
    xsel
    xterm
    (pass.withExtensions (ext: with ext; [ pass-otp ]))
    firefox
    ungoogled-chromium
    element-desktop
  ];

  services.xserver = {
    enable = true;
    # causes GDK_PIXBUF_MODULE_FILE to be set in xsession
    gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
    layout = "us,ru";
    xkbOptions = "grp:caps_toggle,grp_led:caps,terminate:ctrl_alt_bksp";
    libinput = {
      enable = true;
      touchpad.disableWhileTyping = true;
    };
    displayManager.xserverArgs = [ "-nolisten local" ];
  };

  home-manager =
    let home = { xresources.properties = import ./xresources.nix; };
    in {
      users = {
        kurnevsky = home;
        ww = home;
      };
    };
}
