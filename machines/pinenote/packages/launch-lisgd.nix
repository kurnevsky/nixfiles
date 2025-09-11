{
  pkgs,
  ...
}:

pkgs.writeShellScriptBin "launch_lisgd.sh" ''
  ${pkgs.psmisc}/bin/killall lisgd || true
  ${pkgs.lisgd}/bin/lisgd -d /dev/input/by-path/platform-fe5e0000.i2c-event \
      -g "4,DU,*,*,R,xournalpp &" \
      -g "3,DU,*,*,R,${pkgs.hrdl-utils}/bin/toggle_onscreen_keyboard.py &" \
      -g "3,UD,*,*,R,${pkgs.dbus}/bin/dbus-send --type=method_call --dest=org.pinenote.ebc_custom / org.pinenote.ebc_custom.GlobalRefresh &" \
      -g "3,LR,*,*,R,sway_workspace goto prev &" \
      -g "3,RL,*,*,R,sway_workspace goto next &" \
      -g "4,LR,*,*,R,sway_workspace move prev &" \
      -g "4,RL,*,*,R,sway_workspace move next &" \
      -g "4,UD,*,*,R,${pkgs.pinenote.toggle-menu}/bin/toggle_menu.sh &" &
''
