{
  pkgs,
  ...
}:

pkgs.writeShellScriptBin "toggle_menu.sh" ''
  ${pkgs.nwg-menu}/bin/nwg-menu -fm ${pkgs.xfce.thunar}/bin/thunar -term ${pkgs.alacritty}/bin/alacritty -va top -isl 64 -iss 64 -s menu-start.css &
''
