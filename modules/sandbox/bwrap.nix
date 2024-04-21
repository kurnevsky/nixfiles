{ bubblewrap, gnused, callPackage, lib, writeShellScriptBin, closureInfo
, xdg-dbus-proxy, writeText, system }:

drv:

{ name, graphics ? false, target-name ? name, unshare-user ? true
, unshare-ipc ? true, unshare-pid ? true, unshare-net ? true, unshare-uts ? true
, unshare-cgroup ? true, etcs ? [ ], pams ? [ ], whitelist ? [ ]
, ro-whitelist ? [ ], blacklist ? [ ], unsetenvs ? [ ], setenvs ? [ ]
, devs ? [ ], syses ? [ ], shared-tmp ? false, camera ? false, args ? [ ]
, extra-deps ? [ ], runtime-deps ? [ ], opengl ? false, opengl32 ? false
, bin-sh ? false, localtime ? false, resolv-conf ? false, ro-media ? false
, media ? false, disable-userns ? true, dbus ? [ ], system-dbus ? [ ]
, flatpak ? false, seccomp ? [
  "_sysctl"
  "acct"
  "add_key"
  "adjtimex"
  "clock_adjtime"
  "create_module"
  "delete_module"
  "fanotify_init"
  "finit_module"
  "get_kernel_syms"
  "init_module"
  "io_cancel"
  "io_destroy"
  "io_getevents"
  "io_setup"
  "io_submit"
  "ioperm"
  "iopl"
  "ioprio_set"
  "kexec_file_load"
  "kexec_load"
  "keyctl"
  "lookup_dcookie"
  "nfsservctl"
  "migrate_pages"
  "modify_ldt"
  "mount"
  "move_pages"
  "perf_event_open"
  "pivot_root"
  "process_vm_readv"
  "process_vm_writev"
  "ptrace"
  "reboot"
  "remap_file_pages"
  "request_key"
  "swapoff"
  "swapon"
  "sysfs"
  "syslog"
  "tuxcall"
  "umount2"
  "uselib"
  "vmsplice"
] }:

assert !(ro-media && media);

let
  sandbox-seccomp = callPackage ./seccomp.nix { } seccomp;
  cinfo = closureInfo { rootPaths = [ drv ] ++ extra-deps; };
  flatpakArchitectures = {
    "x86_64-linux" = "x86_64";
    "aarch64-linux" = "aarch64";
    "i686-linux" = "i386";
  };
  sharedNamespaces = (lib.optional (!unshare-net) "network")
    ++ (lib.optional (!unshare-ipc) "ipc");
  flatpak-info = writeText "flatpak-info" (lib.generators.toINI { } {
    Application = {
      name = "com.sandbox.${target-name}";
      runtime = "runtime/com.nixpak.Platform/${
          flatpakArchitectures.${system} or "unknown-arch-${system}"
        }/1";
    };
    Context.shared = "${lib.concatStringsSep ";" sharedNamespaces};";
  });
  bindFrom = x: if builtins.isAttrs x then x.from else x;
  bindTo = x: if builtins.isAttrs x then x.to else x;
in writeShellScriptBin target-name ''
  set -euETo pipefail
  shopt -s inherit_errexit

  if [ -n "''${UNSANDBOXED-}" ]
  then
    echo "Running in unsandboxed mode!" >&2
    exec ${drv}/bin/${name} "$@"
  fi

  ${lib.concatMapStringsSep "\n"
  (x: "test ! -e ${bindFrom x} && mkdir -p ${bindFrom x}")
  (lib.filter (x: builtins.match ".*/" (bindFrom x) != null)
    (ro-whitelist ++ whitelist))}

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

  mapfile -t ro_whitelist < <(echo -n "''${RO_WHITELIST-}" | grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  mapfile -t whitelist < <(echo -n "''${WHITELIST-}" | grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--bind\n&\n&/')
  mapfile -t blacklist < <(echo -n "''${BLACKLIST-}" | grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--tmpfs\n&/')

  ${lib.optionalString graphics ''
    mapfile -t xauthority < <(echo -n "''${XAUTHORITY-}" | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  ''}

  mapfile -t deps < <(${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/' ${cinfo}/store-paths ${
    lib.concatStringsSep " " runtime-deps
  })

  ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
    FIFO_TMP=$(mktemp -u)
    mkfifo "$FIFO_TMP"
    exec 3<>"$FIFO_TMP"
  ''}

  ${lib.optionalString (dbus != [ ]) ''
    SANDBOX_BUS="$XDG_RUNTIME_DIR/sandbox-bus-$$"
    ${bubblewrap}/bin/bwrap \
      --ro-bind /nix/store /nix/store \
      --bind /tmp /tmp \
      --bind "$XDG_RUNTIME_DIR" "$XDG_RUNTIME_DIR" \
      --bind "$FIFO_TMP" "$FIFO_TMP" \
      ${lib.optionalString flatpak "--ro-bind ${flatpak-info} /.flatpak-info"} \
      --new-session \
      --die-with-parent \
        ${xdg-dbus-proxy}/bin/xdg-dbus-proxy --fd=3 3>"$FIFO_TMP" "$DBUS_SESSION_BUS_ADDRESS" "$SANDBOX_BUS" ${
          lib.concatMapStringsSep " " (x: "--${x}") dbus
        } --filter &
    head -c 1 <&3 > /dev/null
  ''}

  ${lib.optionalString (system-dbus != [ ]) ''
    SANDBOX_SYSTEM_BUS="$XDG_RUNTIME_DIR/sandbox-system-bus-$$"
    ${bubblewrap}/bin/bwrap \
      --ro-bind /nix/store /nix/store \
      --bind "$XDG_RUNTIME_DIR" "$XDG_RUNTIME_DIR" \
      --bind /run/dbus/system_bus_socket /run/dbus/system_bus_socket \
      --bind "$FIFO_TMP" "$FIFO_TMP" \
      ${lib.optionalString flatpak "--ro-bind ${flatpak-info} /.flatpak-info"} \
      --new-session \
      --die-with-parent \
        ${xdg-dbus-proxy}/bin/xdg-dbus-proxy --fd=3 3>"$FIFO_TMP" unix:path=/run/dbus/system_bus_socket "$SANDBOX_SYSTEM_BUS" ${
          lib.concatMapStringsSep " " (x: "--${x}") system-dbus
        } --filter &
    head -c 1 <&3 > /dev/null
  ''}

  ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
    rm "$FIFO_TMP"
  ''}

  exec ${bubblewrap}/bin/bwrap \
       "''${deps[@]}" \
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
       ${
         lib.concatMapStringsSep " " (x: "--ro-bind ${bindFrom x} ${bindTo x}")
         ro-whitelist
       } \
       ${
         lib.concatMapStringsSep " " (x: "--bind ${bindFrom x} ${bindTo x}")
         whitelist
       } \
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
       ${lib.optionalString disable-userns "--disable-userns"} \
       \
       --new-session \
       \
       --cap-drop ALL \
       \
       ${
         lib.optionalString (dbus != [ ]) ''
           --sync-fd 4 4<&3 \
           --bind "$SANDBOX_BUS" "$XDG_RUNTIME_DIR/bus" \
           --setenv DBUS_SESSION_BUS_ADDRESS unix:path="$XDG_RUNTIME_DIR/bus" \
         ''
       } \
       ${
         lib.optionalString (system-dbus != [ ])
         ''--bind "$SANDBOX_SYSTEM_BUS" /run/dbus/system_bus_socket''
       } \
       ${
         lib.optionalString flatpak ''
           --bind "$XDG_RUNTIME_DIR/doc" "$XDG_RUNTIME_DIR/doc" \
           --ro-bind ${flatpak-info} /.flatpak-info \
           --ro-bind ${flatpak-info} "$XDG_RUNTIME_DIR"/flatpak-info \
         ''
       } \
       ${
         lib.optionalString (seccomp != [ ])
         "--seccomp 5 5< ${sandbox-seccomp}/seccomp.bpf"
       } \
       \
       ${drv}/bin/${name} ${lib.concatStringsSep " " args} "$@"
''
