{
  pkgs,
  ...
}:

pkgs.writeShellScriptBin "sway_workspace.sh" ''
  ws=$(${pkgs.sway}/bin/swaymsg -r -t get_workspaces | ${pkgs.jq}/bin/jq '.[] | select((.output == "DPI-1") and .focused) | .num')
  if [[ $2 == "next" ]]; then
  	new_ws="$(echo "$ws" | tr 123 231)"
  	if [[ $1 == "goto" ]]; then
  		${pkgs.sway}/bin/swaymsg workspace "$new_ws"
  	elif [[ $1 == "move" ]]; then
  		${pkgs.sway}/bin/swaymsg move window to workspace "$new_ws"
  	fi
  elif [[ $2 == "prev" ]]; then
  	new_ws="$(echo "$ws" | tr 123 312)"
  	if [[ $1 == "goto" ]]; then
  		${pkgs.sway}/bin/swaymsg workspace "$new_ws"
  	elif [[ $1 == "move" ]]; then
  		${pkgs.sway}/bin/swaymsg move window to workspace "$new_ws"
  	fi
  else
  	exit 1
  fi
''
