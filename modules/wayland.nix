{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
    firefox-wayland-sandboxed
    element-desktop-wayland-sandboxed
  ];
}