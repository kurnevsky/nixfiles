{ pkgs, ... }:

let
  inherit (pkgs) lib;
  runtimePath = lib.makeBinPath (
    with pkgs;
    [
      coreutils
      gawk
      gnugrep
      gnused
      procps
      psmisc
      util-linux
      jq
      sway
      systemd
      glib
      brightnessctl
      bluez
      pulseaudio
      rot8
      waybar
    ]
  );
in
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pinenote-config-sway";
  version = "1.1.1";

  meta = with lib; {
    description = "Sway/waybar config & service integration for the PineNote";
    homepage = "https://git.sr.ht/~phantomas/pinenote-config-sway";
    license = licenses.gpl3Only;
  };

  src = pkgs.fetchFromSourcehut {
    owner = "~phantomas";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-5PD5vi7NEz1FvkcF5D2UX2zbDK26djhX0Ali8rGfQH0=";
  };

  nativeBuildInputs = with pkgs; [ makeWrapper ];

  installPhase = ''
    runHook preInstall

    root=$out/share/pinenote-config
    mkdir -p $root
    cp -r sway waybar $root/

    # Upstream resolves config/script locations via env variables exported by
    # env_util.sh and finds commands via PATH; here everything is known at
    # build time, so bake the store paths into the waybar configs.
    for f in $root/waybar/config $root/waybar/modules/*.json; do
      substituteInPlace "$f" \
        --replace-quiet '$WAYBAR_CONFIG_DIR' "$root/waybar" \
        --replace-quiet '$WAYBAR_SCRIPT_DIR' "$root/waybar/scripts" \
        --replace-quiet '$SWAY_ROTATE_SERVICE' "$root/sway/scripts/sway_rotate.sh" \
        --replace-quiet 'sway_workspace ' '${pkgs.pinenote.sway-workspace}/bin/sway_workspace.sh ' \
        --replace-quiet 'systemctl --user restart pinenote.service' '${pkgs.systemd}/bin/systemctl --user restart pinenote-service-sway.service' \
        --replace-quiet 'swaymsg' '${pkgs.sway}/bin/swaymsg' \
        --replace-quiet 'busctl' '${pkgs.systemd}/bin/busctl' \
        --replace-quiet 'brightnessctl' '${pkgs.brightnessctl}/bin/brightnessctl' \
        --replace-quiet 'pactl' '${pkgs.pulseaudio}/bin/pactl' \
        --replace-quiet 'rfkill' '${pkgs.util-linux}/bin/rfkill' \
        --replace-quiet 'killall' '${pkgs.psmisc}/bin/killall'
    done

    # The env file indirection is not needed since the wrappers below provide
    # the same variables directly.
    substituteInPlace $root/sway/scripts/start_waybar.sh $root/sway/scripts/sway_rotate.sh \
      --replace-fail 'envfile=$($path/env_util.sh get_env)' 'envfile=/dev/null'

    # Only entry points are wrapped: daemon_util.sh, env_util.sh and
    # hint_utils.sh are sourced by other scripts and must stay plain files.
    for script in \
      $root/sway/scripts/ebcmark.sh \
      $root/sway/scripts/rotate_hook.sh \
      $root/sway/scripts/toggle_squeekboard.sh \
      $root/sway/scripts/sway_rotate.sh \
      $root/sway/scripts/start_waybar.sh \
      $root/waybar/scripts/bluetooth_toggle.sh \
      $root/waybar/scripts/driver_mode.sh \
      $root/waybar/scripts/hint.sh \
      $root/waybar/scripts/monitor_hints.sh \
      $root/waybar/scripts/window_hint.sh
    do
      wrapProgram "$script" \
        --prefix PATH : ${runtimePath} \
        --set WAYBAR_CONFIG_DIR "$root/waybar" \
        --set WAYBAR_SCRIPT_DIR "$root/waybar/scripts" \
        --set SWAY_SCRIPT_DIR "$root/sway/scripts" \
        --set SWAY_ROTATE_SERVICE "$root/sway/scripts/sway_rotate.sh" \
        --set SWAY_ROTATE_HOOK "$root/sway/scripts/rotate_hook.sh"
    done

    runHook postInstall
  '';
}
