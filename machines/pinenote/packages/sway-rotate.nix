{
  pkgs,
  ...
}:

pkgs.writeShellScriptBin "sway_rotate.sh" ''
  focusedtransform() {
  	${pkgs.sway}/bin/swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | .transform'
  }

  focusedname() {
  	${pkgs.sway}/bin/swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | .name'
  }

  startlisgd() {
  	${pkgs.pinenote.launch-lisgd}/bin/launch_lisgd.sh &
  }

  rotnormal() {
  	${pkgs.sway}/bin/swaymsg -- output "-" transform 0 scale 1
  	focused_name="$(focusedname)"
  	${pkgs.sway}/bin/swaymsg -- input type:touch map_to_output "$focused_name"
  	${pkgs.sway}/bin/swaymsg -- input type:tablet_tool map_to_output "$focused_name"
  	startlisgd 0
  	exit 0
  }

  rotleft() {
  	${pkgs.sway}/bin/swaymsg -- output "-" transform 90 scale 1
  	focused_name="$(focusedname)"
  	${pkgs.sway}/bin/swaymsg -- input type:touch map_to_output "$focused_name"
  	${pkgs.sway}/bin/swaymsg -- input type:tablet_tool map_to_output "$focused_name"
  	startlisgd 3
  	exit 0
  }

  rotright() {
  	${pkgs.sway}/bin/swaymsg -- output "-" transform 270 scale 1
  	focused_name="$(focusedname)"
  	${pkgs.sway}/bin/swaymsg -- input type:touch map_to_output "$focused_name"
  	${pkgs.sway}/bin/swaymsg -- input type:tablet_tool map_to_output "$focused_name"
  	startlisgd 1
  	exit 0
  }

  rotinvert() {
  	${pkgs.sway}/bin/swaymsg -- output "-" transform 180 scale 1
  	focused_name="$(focusedname)"
  	${pkgs.sway}/bin/swaymsg -- input type:touch map_to_output "$focused_name"
  	${pkgs.sway}/bin/swaymsg -- input type:tablet_tool map_to_output "$focused_name"
  	startlisgd 2
  	exit 0
  }

  "$@"
''
