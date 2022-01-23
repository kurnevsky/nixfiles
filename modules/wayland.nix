{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
    firefox-wayland
    ungoogled-chromium-wayland
    element-desktop-wayland
  ];
}
