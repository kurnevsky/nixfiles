{
  bubblewrap,
  coreutils,
  findutils,
  gnugrep,
  gnused,
  callPackage,
  lib,
  writeShellScript,
  writeShellScriptBin,
  closureInfo,
  socat,
  xdg-dbus-proxy,
  writeText,
  stdenv,
}:

drv:

{
  name,
  graphics ? false,
  target-name ? name,
  unshare-user ? true,
  unshare-ipc ? true,
  unshare-pid ? true,
  unshare-net ? true,
  unshare-uts ? true,
  unshare-cgroup ? true,
  etcs ? [ ],
  pams ? [ ],
  whitelist ? [ ],
  ro-whitelist ? [ ],
  overlay-whitelist ? [ ],
  blacklist ? [ ],
  unsetenvs ? [ ],
  setenvs ? [ ],
  devs ? [ ],
  syses ? [ ],
  shared-tmp ? false,
  camera ? false,
  args ? [ ],
  whole-store ? false,
  extra-deps ? [ ],
  runtime-deps ? [ ],
  opengl ? false,
  opengl32 ? false,
  pipewire ? false,
  pulse ? pipewire,
  bin-sh ? false,
  localtime ? false,
  resolv-conf ? false,
  # Ports the sandbox listens on that should be reachable from the host
  # loopback. Either `port` or `host-port:sandbox-port`. Overridable at runtime
  # with the PORTS environment variable (whitespace separated, empty to
  # disable).
  ports ? [ ],
  ro-media ? false,
  media ? false,
  disable-userns ? true,
  # Kill the sandbox when the launching process dies: --new-session detaches it
  # from the terminal session, so otherwise Ctrl-C only reaches the outer bwrap
  # and leaves an unkillable sandbox behind (it's pid 1 in its pid namespace).
  die-with-parent ? true,
  dbus ? [ ],
  system-dbus ? [ ],
  flatpak ? false,
  seccomp ? [
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
    "kexec_file_load"
    "kexec_load"
    "keyctl"
    "lookup_dcookie"
    "nfsservctl"
    "migrate_pages"
    "modify_ldt"
    "move_pages"
    "perf_event_open"
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
    "uselib"
    "vmsplice"
  ],
}:

assert !(ro-media && media);
assert lib.assertMsg (
  unshare-user || !disable-userns
) "sandbox ${target-name}: disable-userns requires unshare-user";
assert lib.assertMsg (
  ports == [ ] || unshare-net
) "sandbox ${target-name}: ports require unshare-net";
assert lib.assertMsg (lib.all (
  x: builtins.match "[0-9]+(:[0-9]+)?" (toString x) != null
) ports) "sandbox ${target-name}: ports must be `port` or `host-port:sandbox-port`";

let
  sandbox-seccomp = callPackage ./seccomp.nix { } seccomp;
  # Runs inside the sandbox network namespace: exposes each listening port on a
  # unix socket in a directory shared with the host, since the sandbox loopback
  # is not the host loopback.
  port-forwarder = writeShellScript "sandbox-port-forwarder" ''
    set -euETo pipefail
    shopt -s inherit_errexit

    dir=$1
    shift
    ports=$1
    shift

    for port in $ports
    do
      ${socat}/bin/socat \
        UNIX-LISTEN:"$dir/''${port%%:*}",fork,unlink-early \
        TCP:127.0.0.1:"''${port##*:}" &
    done

    exec "$@"
  '';
  cinfo = closureInfo {
    rootPaths = [ drv ] ++ extra-deps ++ lib.optional (ports != [ ]) port-forwarder;
  };
  flatpakArchitectures = {
    "x86_64-linux" = "x86_64";
    "aarch64-linux" = "aarch64";
    "i686-linux" = "i386";
  };
  sharedNamespaces = (lib.optional (!unshare-net) "network") ++ (lib.optional (!unshare-ipc) "ipc");
  flatpak-info = writeText "flatpak-info" (
    lib.generators.toINI { } {
      Application = {
        name = "com.sandbox.${target-name}";
        runtime = "runtime/com.sandbox.Platform/${
          flatpakArchitectures.${stdenv.hostPlatform.system} or "unknown-arch-${stdenv.hostPlatform.system}"
        }/1";
      };
      Instance.instance-id = target-name;
      Context.shared = "${lib.concatStringsSep ";" sharedNamespaces};";
    }
  );
  bindFrom = x: if builtins.isAttrs x then x.from else x;
  bindTo = x: if builtins.isAttrs x then x.to else x;
in
writeShellScriptBin target-name ''
  set -euETo pipefail
  shopt -s inherit_errexit

  if [ -n "''${ALREADY_SANDBOXED-}" ]
  then
    exec ${drv}/bin/${name} "$@"
  fi

  if [ -n "''${UNSANDBOXED-}" ]
  then
    echo "Running in unsandboxed mode!" >&2
    exec ${drv}/bin/${name} "$@"
  fi

  ${lib.concatMapStringsSep "\n"
    (x: "test ! -e ${bindFrom x} && ${coreutils}/bin/mkdir -p ${bindFrom x}")
    (
      lib.filter (x: builtins.match ".*/" (bindFrom x) != null) (
        ro-whitelist ++ overlay-whitelist ++ whitelist
      )
    )
  }

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
    if [ -n "''${TORJAIL-}" ]
    then
      resolvconf=(--ro-bind /etc/resolv-torjail.conf /etc/resolv.conf)
    elif [ -n "''${DNS-}" ]
    then
      exec 7<<< "nameserver $DNS"
      resolvconf=(--ro-bind-data 7 /etc/resolv.conf)
    else
      resolvconf=(--ro-bind /etc/resolv.conf /etc/resolv.conf)
    fi
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
        ${findutils}/bin/find /dev -maxdepth 1 -type c -regex '/dev/video[0-9]+'
      fi | ${gnused}/bin/sed 's/.*/--dev-bind\n&\n&/'
    )
  ''}

  mapfile -t ro_whitelist < <(echo -n "''${RO_WHITELIST-}" | ${gnugrep}/bin/grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  mapfile -t overlay_whitelist < <(echo -n "''${OVERLAY_WHITELIST-}" | ${gnugrep}/bin/grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--overlay-src\n&\n--tmp-overlay\n&/')
  mapfile -t whitelist < <(echo -n "''${WHITELIST-}" | ${gnugrep}/bin/grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--bind\n&\n&/')
  mapfile -t blacklist < <(echo -n "''${BLACKLIST-}" | ${gnugrep}/bin/grep -v '^[[:space:]]*$' | ${gnused}/bin/sed 's/.*/--tmpfs\n&/')

  ${lib.optionalString graphics ''
    mapfile -t xauthority < <(echo -n "''${XAUTHORITY-}" | ${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/')
  ''}

  ${lib.optionalString (!whole-store) ''
    mapfile -t deps < <(${gnused}/bin/sed 's/.*/--ro-bind\n&\n&/' ${cinfo}/store-paths ${lib.concatStringsSep " " runtime-deps})
  ''}

  ${lib.optionalString (ports != [ ]) ''
    port_binds=()
    port_forwarder=()

    # with a shared network namespace the ports are already reachable
    if [ -z "''${WITH_NETWORK-}" ]
    then
      mapfile -t ports < <(
        echo -n "''${PORTS-${lib.concatMapStringsSep " " toString ports}}" |
          ${gnused}/bin/sed 's/[[:space:]]\+/\n/g' |
          ${gnugrep}/bin/grep -v '^$'
      )

      for port in "''${ports[@]}"
      do
        if [[ ! $port =~ ^[0-9]+(:[0-9]+)?$ ]]
        then
          echo "${target-name}: invalid port: $port" >&2
          exit 1
        fi
      done

      if [ "''${#ports[@]}" -gt 0 ]
      then
        SANDBOX_PORTS="$XDG_RUNTIME_DIR/sandbox-ports-${target-name}"
        ${coreutils}/bin/mkdir -p "$SANDBOX_PORTS"

        for port in "''${ports[@]}"
        do
          ${bubblewrap}/bin/bwrap \
            --ro-bind /nix/store /nix/store \
            --bind "$SANDBOX_PORTS" "$SANDBOX_PORTS" \
            --new-session \
            --die-with-parent \
              ${socat}/bin/socat \
                TCP-LISTEN:"''${port%%:*}",bind=127.0.0.1,reuseaddr,fork \
                UNIX-CONNECT:"$SANDBOX_PORTS/''${port%%:*}" &
        done

        port_binds=(--bind "$SANDBOX_PORTS" "$SANDBOX_PORTS")
        port_forwarder=(${port-forwarder} "$SANDBOX_PORTS" "''${ports[*]}")
      fi
    fi
  ''}

  ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
    FIFO_TMP=$(${coreutils}/bin/mktemp -u)
    ${coreutils}/bin/mkfifo "$FIFO_TMP"
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
    ${coreutils}/bin/head -c 1 <&3 > /dev/null
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
    ${coreutils}/bin/head -c 1 <&3 > /dev/null
  ''}

  ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
    ${coreutils}/bin/rm "$FIFO_TMP"
  ''}

  ${lib.optionalString flatpak ''
    ${coreutils}/bin/mkdir -p "$XDG_RUNTIME_DIR/.flatpak/${target-name}/"
  ''}

  exec ${bubblewrap}/bin/bwrap \
       ${if whole-store then "--ro-bind /nix/store/ /nix/store/" else ''"''${deps[@]}"''} \
       \
       ${lib.optionalString bin-sh "--ro-bind /bin/sh /bin/sh"} \
       \
       --proc /proc \
       \
       --dev /dev \
       ${lib.concatMapStringsSep " " (x: "--dev-bind /dev/${x} /dev/${x}") devs} \
       ${lib.optionalString camera ''"''${video[@]}"''} \
       \
       ${lib.concatMapStringsSep " " (x: "--ro-bind /sys/${x} /sys/${x}") syses} \
       \
       --tmpfs /run \
       --ro-bind /run/current-system/sw /run/current-system/sw \
       ${lib.optionalString opengl "--ro-bind /run/opengl-driver /run/opengl-driver"} \
       ${lib.optionalString opengl32 "--ro-bind /run/opengl-driver-32 /run/opengl-driver-32"} \
       ${lib.optionalString pipewire "--bind /run/pipewire /run/pipewire --bind /var/run/pipewire /var/run/pipewire"} \
       ${lib.optionalString pulse "--bind /run/pulse /run/pulse --bind /var/run/pulse /var/run/pulse"} \
       \
       ${
         lib.concatMapStringsSep " " (x: ''--bind-try "$XDG_RUNTIME_DIR"/${x} "$XDG_RUNTIME_DIR"/${x}'') pams
       } \
       ${lib.optionalString graphics ''--bind-try "$XDG_RUNTIME_DIR"/"''${WAYLAND_DISPLAY-wayland-0}" "$XDG_RUNTIME_DIR"/"''${WAYLAND_DISPLAY-wayland-0}"''} \
       \
       --ro-bind /etc/profiles/per-user/"$(${coreutils}/bin/whoami)" /etc/profiles/per-user/"$(${coreutils}/bin/whoami)" \
       ${lib.concatMapStringsSep " " (x: "--ro-bind /etc/${x} /etc/${x}") etcs} \
       ${lib.optionalString localtime ''"''${localtime[@]}"''} \
       ${lib.optionalString resolv-conf ''"''${resolvconf[@]}"''} \
       \
       ${if shared-tmp then "--bind /tmp /tmp" else "--tmpfs /tmp"} \
       ${lib.optionalString (graphics && !shared-tmp) "--bind /tmp/.X11-unix /tmp/.X11-unix"} \
       \
       ${lib.optionalString ro-media ''--ro-bind-try /run/media/"$(${coreutils}/bin/whoami)" /run/media/"$(${coreutils}/bin/whoami)"''} \
       ${lib.optionalString media ''--bind-try /run/media/"$(${coreutils}/bin/whoami)" /run/media/"$(${coreutils}/bin/whoami)"''} \
       \
       ${lib.concatMapStringsSep " " (x: "--ro-bind ${bindFrom x} ${bindTo x}") ro-whitelist} \
       ${
         lib.concatMapStringsSep " " (
           x: "--overlay-src ${bindFrom x} --tmp-overlay ${bindTo x}"
         ) overlay-whitelist
       } \
       ${lib.concatMapStringsSep " " (x: "--bind ${bindFrom x} ${bindTo x}") whitelist} \
       ${lib.concatMapStringsSep " " (x: "--tmpfs ${x}") blacklist} \
       \
       ${lib.optionalString graphics ''"''${xauthority[@]}"''} \
       \
       "''${ro_whitelist[@]}" \
       "''${overlay_whitelist[@]}" \
       "''${whitelist[@]}" \
       "''${blacklist[@]}" \
       ${lib.optionalString (ports != [ ]) ''"''${port_binds[@]}"''} \
       \
       --setenv ALREADY_SANDBOXED 1 \
       ${lib.concatMapStringsSep " " (x: "--unsetenv ${x}") unsetenvs} \
       ${lib.concatMapStringsSep " " (x: "--setenv ${x.name} ${x.value}") setenvs} \
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
       ${lib.optionalString die-with-parent "--die-with-parent"} \
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
         lib.optionalString (system-dbus != [ ]) ''--bind "$SANDBOX_SYSTEM_BUS" /run/dbus/system_bus_socket''
       } \
       ${lib.optionalString flatpak ''
         --info-fd 5 5>"$XDG_RUNTIME_DIR/.flatpak/${target-name}/bwrapinfo.json" \
         --bind "$XDG_RUNTIME_DIR/doc" "$XDG_RUNTIME_DIR/doc" \
         --ro-bind ${flatpak-info} /.flatpak-info \
         --ro-bind ${flatpak-info} "$XDG_RUNTIME_DIR"/flatpak-info \
       ''} \
       ${lib.optionalString (seccomp != [ ]) "--seccomp 6 6< ${sandbox-seccomp}/seccomp.bpf"} \
       \
       ${lib.optionalString (ports != [ ]) ''"''${port_forwarder[@]}"''} \
       ${drv}/bin/${name} ${lib.concatStringsSep " " args} "$@"
''
