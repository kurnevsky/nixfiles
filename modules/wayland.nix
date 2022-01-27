{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    ungoogled-chromium-wayland
    element-desktop-wayland
  ];
}
