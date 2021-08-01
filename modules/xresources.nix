let
  base00 = "#282c34";
  base01 = "#353b45";
  base02 = "#3e4451";
  base03 = "#545862";
  base04 = "#565c64";
  base05 = "#abb2bf";
  base06 = "#b6bdca";
  base07 = "#c8ccd4";
  base08 = "#e06c75";
  base09 = "#d19a66";
  base0A = "#e5c07b";
  base0B = "#98c379";
  base0C = "#56b6c2";
  base0D = "#61afef";
  base0E = "#c678dd";
  base0F = "#be5046";
in {
  "*foreground" = base05;
  "*background" = base00;
  "*cursorColor" = base05;
  "*color0" = base00;
  "*color1" = base08;
  "*color2" = base0B;
  "*color3" = base0A;
  "*color4" = base0D;
  "*color5" = base0E;
  "*color6" = base0C;
  "*color7" = base05;
  "*color8" = base03;
  "*color9" = base09;
  "*color10" = base01;
  "*color11" = base02;
  "*color12" = base04;
  "*color13" = base06;
  "*color14" = base0F;
  "*color15" = base07;
  "Xft.dpi" = 96;
  "Xft.antialias" = true;
  "Xft.rgba" = "rgb";
  "Xft.hinting" = true;
  "Xft.hintstyle" = "hintslight";
  "XTerm.termName" = "xterm-256color";
  "XTerm.vt100.saveLines" = 0;
  "XTerm.vt100.reverseVideo" = false;
  "XTerm.vt100.faceName" =
    "xft:DejaVu Sans Mono:size=12:antialias=true:autohint=false";
  "XTerm.vt100.bellIsUrgent" = true;
  "XTerm.vt100.metaSendsEscape" = true;
  # Send C-? instead of C-h for the backspace key
  "XTerm.ttyModes" = "erase ^?";
  "XTerm.vt100.translations" = "#override <Key>BackSpace: string(0x7f)";
  "XTerm.backarrowKeyIsErase" = true;
}
