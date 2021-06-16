{ bubblewrap, lib, sandbox-seccomp, writeShellScriptBin }:

drv:

{ name, x11 ? false, target-name ? name, unshare-user ? true, unshare-ipc ? true
, unshare-pid ? true, unshare-net ? true, unshare-uts ? true
, unshare-cgroup ? true, etcs ? [ ], pams ? [ ], whitelist ? [ ]
, ro-whitelist ? [ ], blacklist ? [ ], unsetenvs ? [ ], setenvs ? [ ]
, devs ? [ ], syses ? [ ], shared-tmp ? false, camera ? false, args ? [ ]
, system-bus-socket ? false }:

# TODO: use buildInputs to determine paths to bind
# TODO: find out what from /run/current-system is actually needed

writeShellScriptBin target-name ''
  set -euETo pipefail
  shopt -s inherit_errexit

  if [ ! -z ''${UNSANDBOXED-} ]
  then
    echo "Running in unsandboxed mode!"
    exec ${drv}/bin/${name} "$@"
  fi

  ${lib.concatMapStringsSep "\n" (x: "test ! -e ${x} && mkdir -p ${x}")
  (ro-whitelist ++ whitelist)}

  ${lib.optionalString camera ''
    mapfile -t video < <(find /dev -maxdepth 1 -type c -regex '/dev/video[0-9]+' | sed 's/.*/--dev-bind\n&\n&/')
  ''}

  exec ${bubblewrap}/bin/bwrap \
       --ro-bind /nix /nix \
       \
       --proc /proc \
       \
       --dev /dev \
       ${
         lib.concatMapStringsSep " " (x: "--dev-bind /dev/${x} /dev/${x}") devs
       } \
       ${lib.optionalString camera "\${video[@]}"} \
       \
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind /sys/${x} /sys/${x}") syses
       } \
       \
       --tmpfs /run \
       --ro-bind /run/current-system /run/current-system \
       \
       ${
         lib.optionalString system-bus-socket
         "--bind /run/dbus/system_bus_socket /run/dbus/system_bus_socket"
       } \
       ${
         lib.concatMapStringsSep " "
         (x: "--bind /run/user/$UID/${x} /run/user/$UID/${x}") pams
       } \
       \
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind /etc/${x} /etc/${x}") etcs
       } \
       \
       ${lib.optionalString shared-tmp "--bind /tmp /tmp"} \
       ${
         lib.optionalString (x11 && !shared-tmp)
         "--bind /tmp/.X11-unix /tmp/.X11-unix"
       } \
       \
       ${lib.concatMapStringsSep " " (x: "--ro-bind ${x} ${x}") ro-whitelist} \
       ${lib.concatMapStringsSep " " (x: "--bind ${x} ${x}") whitelist} \
       ${lib.concatMapStringsSep " " (x: "--tmpfs ${x}") blacklist} \
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
       ${lib.optionalString unshare-net "--unshare-net"} \
       ${lib.optionalString unshare-uts "--unshare-uts"} \
       ${lib.optionalString unshare-cgroup "--unshare-cgroup"} \
       \
       --new-session \
       --die-with-parent \
       \
       --cap-drop ALL \
       \
       --seccomp 3 \
       3< ${sandbox-seccomp}/seccomp.bpf \
       \
       ${drv}/bin/${name} ${lib.concatStringsSep " " args} "$@"
''
