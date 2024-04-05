{ writeShellScriptBin }:

bin:

writeShellScriptBin (builtins.baseNameOf bin) ''
  set -euETo pipefail
  shopt -s inherit_errexit

  case $TERM in
    xterm-256color)
      typeset -g TERM=xterm-direct
      ;;
    konsole)
      typeset -g TERM=konsole-direct
      ;;
    alacritty)
      typeset -g TERM=alacritty-direct
      ;;
  esac

  exec ${bin} "$@"
''
