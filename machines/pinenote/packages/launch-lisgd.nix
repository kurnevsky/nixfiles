{
  pkgs,
  ...
}:

pkgs.writeShellScriptBin "launch_lisgd.sh" ''
  ${pkgs.psmisc}/bin/killall lisgd || true
  ${pkgs.lisgd}/bin/lisgd -d /dev/input/by-path/platform-fe5e0000.i2c-event \
      -g "1,RL,R,S,P,${pkgs.sway}/bin/swaymsg bar hidden_state show bar-ctrl" \
      -g "1,LR,R,S,P,${pkgs.sway}/bin/swaymsg bar hidden_state hide bar-ctrl" \
      -g "4,DU,*,*,R,${pkgs.xournalpp}/bin/xournalpp &" \
      -g "3,DU,*,*,R,${pkgs.hrdl-utils}/bin/toggle_onscreen_keyboard.py &" \
      -g "3,UD,*,*,R,${pkgs.systemd}/bin/busctl --user call org.pinenote.PineNoteCtl /org/pinenote/PineNoteCtl org.pinenote.Ebc1 GlobalRefresh &" \
      -g "3,LR,*,*,R,${pkgs.pinenote.sway-workspace}/bin/sway_workspace.sh goto prev &" \
      -g "3,RL,*,*,R,${pkgs.pinenote.sway-workspace}/bin/sway_workspace.sh goto next &" \
      -g "4,LR,*,*,R,${pkgs.pinenote.sway-workspace}/bin/sway_workspace.sh move prev &" \
      -g "4,RL,*,*,R,${pkgs.pinenote.sway-workspace}/bin/sway_workspace.sh move next &" \
      -g "4,UD,*,*,R,${pkgs.pinenote.toggle-menu}/bin/toggle_menu.sh &" &
''
