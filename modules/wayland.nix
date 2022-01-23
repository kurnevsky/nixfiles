{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    firefox-wayland
    ungoogled-chromium-wayland
    element-desktop-wayland
  ];
}
