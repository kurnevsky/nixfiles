{ bubblewrap, gnused, callPackage, lib, writeShellScriptBin, closureInfo }:

drv:

{ name, graphics ? false, target-name ? name, unshare-user ? true
, unshare-ipc ? true, unshare-pid ? true, unshare-net ? true, unshare-uts ? true
, unshare-cgroup ? true, etcs ? [ ], pams ? [ ], whitelist ? [ ]
, ro-whitelist ? [ ], blacklist ? [ ], unsetenvs ? [ ], setenvs ? [ ]
, devs ? [ ], syses ? [ ], shared-tmp ? false, camera ? false, args ? [ ]
, system-bus-socket ? false, extra-deps ? [ ], extra-deps-no-transitive ? [ ]
, opengl ? false, opengl32 ? false, seccomp ? true, bin-sh ? false
, localtime ? false, resolv-conf ? false, ro-media ? false, media ? false }:

assert !(ro-media && media);

let
  sandbox-seccomp = callPackage ./seccomp.nix { };
  cinfo = closureInfo { rootPaths = [ drv ] ++ extra-deps; };
in writeShellScriptBin target-name ''
  set -euETo pipefail
  shopt -s inherit_errexit

  if [ -n "''${UNSANDBOXED-}" ]
  then
    echo "Running in unsandboxed mode!" >&2
    exec ${drv}/bin/${name} "$@"
  fi

  ${lib.concatMapStringsSep "\n" (x: "test ! -e ${x} && mkdir -p ${x}")
  (lib.filter (s: builtins.match ".*/" s != null) (ro-whitelist ++ whitelist))}

  ${lib.optionalString unshare-net ''
    mapfile -t unshare_net < <(
      if [ -z "''${WITH_NETWORK-}" ]
      then
        echo '--unshare-net'
      fi
    )
  ''}

  ${lib.optionalString (resolv-conf && localtime) ''
    if [[ ! -v NOLOCALTIME ]] && [[ -v TORJAIL ]]
    then
      NOLOCALTIME="$TORJAIL"
    fi
  ''}

  ${lib.optionalString resolv-conf ''
    mapfile -t resolvconf < <(
      echo '--ro-bind'
      if [ -n "''${TORJAIL-}" ]
      then
        echo '/etc/resolv-torjail.conf'
      elif [ -n "''${DNS-}" ]
      then
        RESOLV_TMP=$(mktemp)
        echo "nameserver $DNS" > "$RESOLV_TMP"
        echo "$RESOLV_TMP"
      else
        echo '/etc/resolv.conf'
      fi
      echo '/etc/resolv.conf'
    )
  ''}

  ${lib.optionalString localtime ''
    mapfile -t localtime < <(
      if [ -z "''${NOLOCALTIME-}" ]
      then
        echo '--ro-bind'
        echo '/etc/localtime'
        echo '/etc/localtime'
      fi
    )
  ''}

  ${lib.optionalString camera ''
    mapfile -t video < <(
      if [[ -v CAMERA ]]
      then
        echo -n "$CAMERA"
      else
        find /dev -maxdepth 1 -type c -regex '/dev/video[0-9]+'
      fi | ${gnused}/bin/sed 's/.*/--dev-bind\n&\n&/'
    )
  ''}

  mapfile -t ro_whitelist < <(echo -n "''${RO_WHITELIST-}" | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  mapfile -t whitelist < <(echo -n "''${WHITELIST-}" | ${gnused}/bin/sed 's/.*/--bind\n&\n&/')
  mapfile -t blacklist < <(echo -n "''${BLACKLIST-}" | ${gnused}/bin/sed 's/.*/--tmpfs\n&/')

  ${lib.optionalString graphics ''
    mapfile -t xauthority < <(echo -n "''${XAUTHORITY-}" | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  ''}

  mapfile -t deps < <(${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/' ${cinfo}/store-paths)

  exec ${bubblewrap}/bin/bwrap \
       "''${deps[@]}" \
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind ${x} ${x}")
         extra-deps-no-transitive
       } \
       \
       ${lib.optionalString bin-sh "--ro-bind /bin/sh /bin/sh"} \
       \
       --proc /proc \
       \
       --dev /dev \
       ${
         lib.concatMapStringsSep " " (x: "--dev-bind /dev/${x} /dev/${x}") devs
       } \
       ${lib.optionalString camera ''"''${video[@]}"''} \
       \
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind /sys/${x} /sys/${x}") syses
       } \
       \
       --tmpfs /run \
       --ro-bind /run/current-system/sw /run/current-system/sw \
       ${
         lib.optionalString opengl
         "--ro-bind /run/opengl-driver /run/opengl-driver"
       } \
       ${
         lib.optionalString opengl32
         "--ro-bind /run/opengl-driver-32 /run/opengl-driver-32"
       } \
       \
       ${
         lib.optionalString system-bus-socket
         "--bind /run/dbus/system_bus_socket /run/dbus/system_bus_socket"
       } \
       ${
         lib.concatMapStringsSep " "
         (x: ''--bind-try "$XDG_RUNTIME_DIR"/${x} "$XDG_RUNTIME_DIR"/${x}'')
         pams
       } \
       ${
         lib.optionalString graphics ''
           --bind-try "$XDG_RUNTIME_DIR"/"''${WAYLAND_DISPLAY-wayland-0}" "$XDG_RUNTIME_DIR"/"''${WAYLAND_DISPLAY-wayland-0}"''
       } \
       \
       --ro-bind /etc/profiles/per-user/"$(whoami)" /etc/profiles/per-user/"$(whoami)" \
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind /etc/${x} /etc/${x}") etcs
       } \
       ${lib.optionalString localtime ''"''${localtime[@]}"''} \
       ${lib.optionalString resolv-conf ''"''${resolvconf[@]}"''} \
       \
       ${lib.optionalString shared-tmp "--bind /tmp /tmp"} \
       ${
         lib.optionalString (graphics && !shared-tmp)
         "--bind /tmp/.X11-unix /tmp/.X11-unix"
       } \
       \
       ${
         lib.optionalString ro-media
         ''--ro-bind-try /run/media/"$(whoami)" /run/media/"$(whoami)"''
       } \
       ${
         lib.optionalString media
         ''--bind-try /run/media/"$(whoami)" /run/media/"$(whoami)"''
       } \
       \
       ${lib.concatMapStringsSep " " (x: "--ro-bind ${x} ${x}") ro-whitelist} \
       ${lib.concatMapStringsSep " " (x: "--bind ${x} ${x}") whitelist} \
       ${lib.concatMapStringsSep " " (x: "--tmpfs ${x}") blacklist} \
       \
       ${lib.optionalString graphics ''"''${xauthority[@]}"''} \
       \
       "''${ro_whitelist[@]}" \
       "''${whitelist[@]}" \
       "''${blacklist[@]}" \
       \
       ${lib.concatMapStringsSep " " (x: "--unsetenv ${x}") unsetenvs} \
       ${
         lib.concatMapStringsSep " " (x: "--setenv ${x.name} ${x.value}")
         setenvs
       } \
       \
       ${lib.optionalString unshare-user "--unshare-user"} \
       ${lib.optionalString unshare-ipc "--unshare-ipc"} \
       ${lib.optionalString unshare-pid "--unshare-pid"} \
       ${lib.optionalString unshare-net ''"''${unshare_net[@]}"''} \
       ${lib.optionalString unshare-uts "--unshare-uts"} \
       ${lib.optionalString unshare-cgroup "--unshare-cgroup"} \
       \
       --disable-userns \
       \
       --new-session \
       \
       --cap-drop ALL \
       \
       ${
         lib.optionalString seccomp
         "--seccomp 3 3< ${sandbox-seccomp}/seccomp.bpf"
       } \
       \
       ${drv}/bin/${name} ${lib.concatStringsSep " " args} "$@"
''
