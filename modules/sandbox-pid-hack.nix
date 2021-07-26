{ dbus, writeShellScriptBin }:

drv: name:

# StatusNotifierItem protocol specifies PID as unique ID of dbus service used
# for tray icons. This wrapper makes sure that PIDs don't collide across
# different PID namespaces.
writeShellScriptBin name ''
  declare -A occupied=()

  while read -r name
  do
    [[ $name =~ StatusNotifierItem-([0-9]+) ]] && occupied["''${BASH_REMATCH[1]}"]=1
  done < <(
    ${dbus}/bin/dbus-send --print-reply --dest=org.freedesktop.DBus /org/freedesktop/DBus org.freedesktop.DBus.ListNames
  )

  if [ ''${occupied[$BASHPID]} ]
  then
    unset -v occupied
    $0 "$@"
  else
    exec ${drv}/bin/${name} "$@"
  fi
''
