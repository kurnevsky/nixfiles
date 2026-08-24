{
  hakoniwa,
  bash,
  coreutils,
  callPackage,
  lib,
  writeShellScriptBin,
  closureInfo,
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
  ro-media ? false,
  media ? false,
  disable-userns ? true,
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
  ],
}:

assert !(ro-media && media);

let
  sandbox-seccomp = callPackage ./seccomp.nix { } {
    blacklist = seccomp;
    inherit disable-userns;
  };
  cinfo = closureInfo { rootPaths = [ drv ] ++ extra-deps; };
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
  # hakoniwa closes all file descriptors above stderr in the container, so the
  # proxy has to open the readiness fifo on its own
  proxy-wrapper = "${bash}/bin/bash -c 'exec 3>\"$1\"; shift; exec \"$@\"' -- \"$FIFO_TMP\"";
  proxy-sandbox = ''
    ${hakoniwa}/bin/hakoniwa run \
      --rootfs=none \
      --seccomp unconfined \
      --bindmount-ro=/nix/store \
      --bindmount-rw=/tmp \
      --bindmount-rw="$XDG_RUNTIME_DIR" \
      --bindmount-rw="$FIFO_TMP" \
      ${lib.optionalString flatpak "--bindmount-ro=${flatpak-info}:/.flatpak-info"} \
      --setenv HOME --setenv XDG_RUNTIME_DIR --setenv DBUS_SESSION_BUS_ADDRESS'';
in
# hakoniwa always creates a new PID namespace, there is no way to opt out of it
lib.warnIf (!unshare-pid)
  "sandbox ${target-name}: unshare-pid = false is not supported by hakoniwa, a new PID namespace is created anyway"
  (
    lib.warnIf (!unshare-user)
      "sandbox ${target-name}: unshare-user = false is not supported by hakoniwa, a new user namespace is created anyway"
      (
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

          ${lib.concatMapStringsSep "\n" (x: "test ! -e ${bindFrom x} && mkdir -p ${bindFrom x}") (
            lib.filter (x: builtins.match ".*/" (bindFrom x) != null) (
              ro-whitelist ++ overlay-whitelist ++ whitelist
            )
          )}

          # mounts of the container and the container side paths they are mounted on
          mounts=()
          targets=()

          bind() {
            mounts+=("$1=$2:$3")
            targets+=("$3")
          }

          bind_ro() {
            bind --bindmount-ro "$1" "''${2-$1}"
          }

          bind_rw() {
            bind --bindmount-rw "$1" "''${2-$1}"
          }

          # unlike bubblewrap hakoniwa fails on a missing mount source
          bind_ro_try() {
            if [ -e "$1" ]
            then
              bind_ro "$1" "''${2-$1}"
            fi
          }

          bind_rw_try() {
            if [ -e "$1" ]
            then
              bind_rw "$1" "''${2-$1}"
            fi
          }

          hide() {
            mounts+=("--tmpfs=$1")
            targets+=("$1")
          }

          ${lib.optionalString (!whole-store) ''
            mapfile -t deps < <(grep -hv '^[[:space:]]*$' ${cinfo}/store-paths ${
              lib.concatStringsSep " " runtime-deps
            })
            deps=("''${deps[@]/#/--bindmount-ro=}")
          ''}

          ${lib.optionalString whole-store "bind_ro /nix/store"}
          ${lib.optionalString bin-sh "bind_ro /bin/sh"}

          mounts+=(--devfs=/dev)
          ${lib.concatMapStringsSep "\n" (x: "bind_rw /dev/${x}") devs}

          ${lib.optionalString camera ''
            while IFS= read -r camera_dev
            do
              if [ -c "$camera_dev" ]
              then
                bind_rw "$camera_dev"
              fi
            done < <(
              if [[ -v CAMERA ]]
              then
                echo -n "$CAMERA"
              else
                for dev in /dev/video*
                do
                  echo "$dev"
                done
              fi
            )
          ''}

          ${lib.concatMapStringsSep "\n" (x: "bind_ro /sys/${x}") syses}

          hide /run
          bind_ro /run/current-system/sw
          ${lib.optionalString opengl "bind_ro /run/opengl-driver"}
          ${lib.optionalString opengl32 "bind_ro /run/opengl-driver-32"}
          ${lib.optionalString pipewire ''
            bind_rw /run/pipewire
            bind_rw /var/run/pipewire
          ''}
          ${lib.optionalString pulse ''
            bind_rw /run/pulse
            bind_rw /var/run/pulse
          ''}

          ${lib.concatMapStringsSep "\n" (x: ''bind_rw_try "$XDG_RUNTIME_DIR"/${x}'') pams}
          ${lib.optionalString graphics ''bind_rw_try "$XDG_RUNTIME_DIR"/"''${WAYLAND_DISPLAY-wayland-0}"''}

          bind_ro /etc/profiles/per-user/"$(${coreutils}/bin/whoami)"
          ${lib.concatMapStringsSep "\n" (x: "bind_ro /etc/${x}") etcs}

          ${lib.optionalString (resolv-conf && localtime) ''
            if [[ ! -v NOLOCALTIME ]] && [[ -v TORJAIL ]]
            then
              NOLOCALTIME="$TORJAIL"
            fi
          ''}

          ${lib.optionalString localtime ''
            if [ -z "''${NOLOCALTIME-}" ]
            then
              bind_ro /etc/localtime
            fi
          ''}

          ${lib.optionalString resolv-conf ''
            if [ -n "''${TORJAIL-}" ]
            then
              bind_ro /etc/resolv-torjail.conf /etc/resolv.conf
            elif [ -n "''${DNS-}" ]
            then
              RESOLV_TMP=$(${coreutils}/bin/mktemp)
              echo "nameserver $DNS" > "$RESOLV_TMP"
              bind_ro "$RESOLV_TMP" /etc/resolv.conf
            else
              bind_ro /etc/resolv.conf
            fi
          ''}

          ${
            # the container root is read-only, so unlike with bubblewrap the
            # sandbox can't create /tmp on its own
            if shared-tmp then "bind_rw /tmp" else "hide /tmp"
          }
          ${lib.optionalString (graphics && !shared-tmp) "bind_rw /tmp/.X11-unix"}

          ${lib.optionalString ro-media ''bind_ro_try /run/media/"$(${coreutils}/bin/whoami)"''}
          ${lib.optionalString media ''bind_rw_try /run/media/"$(${coreutils}/bin/whoami)"''}

          ${lib.concatMapStringsSep "\n" (x: "bind_ro ${bindFrom x} ${bindTo x}") ro-whitelist}
          ${
            # hakoniwa has no counterpart of --tmp-overlay, mount read-only
            lib.concatMapStringsSep "\n" (x: "bind_ro ${bindFrom x} ${bindTo x}") overlay-whitelist
          }
          ${lib.concatMapStringsSep "\n" (x: "bind_rw ${bindFrom x} ${bindTo x}") whitelist}
          ${lib.concatMapStringsSep "\n" (x: "hide ${x}") blacklist}

          ${lib.optionalString graphics ''bind_ro_try "''${XAUTHORITY-}"''}

          while IFS= read -r path
          do
            if [ -n "$path" ]
            then
              bind_ro "$path"
            fi
          done <<< "''${RO_WHITELIST-}''${OVERLAY_WHITELIST+$'\n'}''${OVERLAY_WHITELIST-}"

          while IFS= read -r path
          do
            if [ -n "$path" ]
            then
              bind_rw "$path"
            fi
          done <<< "''${WHITELIST-}"

          while IFS= read -r path
          do
            if [ -n "$path" ]
            then
              hide "$path"
            fi
          done <<< "''${BLACKLIST-}"

          # hakoniwa passes only the explicitly set variables to the container
          envs=()
          while IFS= read -r var
          do
            case "$var" in
              ${lib.concatMapStringsSep "|" (x: "${x}") (unsetenvs ++ [ "ALREADY_SANDBOXED" ])})
                continue
                ;;
            esac
            envs+=("--setenv=$var=''${!var}")
          done < <(compgen -e)
          envs+=(--setenv=ALREADY_SANDBOXED=1)
          ${lib.concatMapStringsSep "\n" (x: "envs+=(--setenv=${x.name}=${x.value})") setenvs}

          # bubblewrap falls back to the root directory when the current one is
          # not available in the sandbox
          workdir=(--workdir=:/ --setenv=PWD=/)
          for target in "''${targets[@]}"
          do
            if [[ "$PWD" == "$target" || "$PWD" == "$target"/* ]]
            then
              workdir=(--workdir=:"$PWD" --setenv=PWD="$PWD")
              break
            fi
          done

          ${lib.optionalString unshare-net ''
            unshare_net=()
            if [ -z "''${WITH_NETWORK-}" ]
            then
              unshare_net=(--unshare-network)
            fi
          ''}

          ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
            FIFO_TMP=$(${coreutils}/bin/mktemp -u)
            ${coreutils}/bin/mkfifo "$FIFO_TMP"
            exec 3<>"$FIFO_TMP"
          ''}

          ${lib.optionalString (dbus != [ ]) ''
            SANDBOX_BUS="$XDG_RUNTIME_DIR/sandbox-bus-$$"
            ${proxy-sandbox} \
              -- ${proxy-wrapper} \
                ${xdg-dbus-proxy}/bin/xdg-dbus-proxy --fd=3 "$DBUS_SESSION_BUS_ADDRESS" "$SANDBOX_BUS" ${
                  lib.concatMapStringsSep " " (x: "--${x}") dbus
                } --filter 3<&- &
            ${coreutils}/bin/head -c 1 <&3 > /dev/null
            bind_rw "$SANDBOX_BUS" "$XDG_RUNTIME_DIR/bus"
            envs+=(--setenv=DBUS_SESSION_BUS_ADDRESS=unix:path="$XDG_RUNTIME_DIR/bus")
          ''}

          ${lib.optionalString (system-dbus != [ ]) ''
            SANDBOX_SYSTEM_BUS="$XDG_RUNTIME_DIR/sandbox-system-bus-$$"
            ${proxy-sandbox} \
              --bindmount-rw=/run/dbus/system_bus_socket \
              -- ${proxy-wrapper} \
                ${xdg-dbus-proxy}/bin/xdg-dbus-proxy --fd=3 unix:path=/run/dbus/system_bus_socket "$SANDBOX_SYSTEM_BUS" ${
                  lib.concatMapStringsSep " " (x: "--${x}") system-dbus
                } --filter 3<&- &
            ${coreutils}/bin/head -c 1 <&3 > /dev/null
            bind_rw "$SANDBOX_SYSTEM_BUS" /run/dbus/system_bus_socket
          ''}

          ${lib.optionalString (dbus != [ ] || system-dbus != [ ]) ''
            ${coreutils}/bin/rm "$FIFO_TMP"
          ''}

          ${lib.optionalString flatpak ''
            mkdir -p "$XDG_RUNTIME_DIR/.flatpak/${target-name}/"
            bind_rw_try "$XDG_RUNTIME_DIR/doc"
            bind_ro ${flatpak-info} /.flatpak-info
            bind_ro ${flatpak-info} "$XDG_RUNTIME_DIR"/flatpak-info
          ''}

          # the proxies exit as soon as the last reader of the fifo is gone, the
          # descriptor is kept open by hakoniwa itself but closed in the container
          exec ${hakoniwa}/bin/hakoniwa run \
               --rootfs=none \
               ${
                 if seccomp != [ ] || disable-userns then
                   "--seccomp ${sandbox-seccomp}"
                 else
                   "--seccomp unconfined"
               } \
               ${lib.optionalString (!whole-store) ''"''${deps[@]}"''} \
               "''${mounts[@]}" \
               "''${envs[@]}" \
               "''${workdir[@]}" \
               ${lib.optionalString unshare-ipc "--unshare-ipc"} \
               ${lib.optionalString unshare-net ''"''${unshare_net[@]}"''} \
               ${lib.optionalString unshare-uts "--unshare-uts"} \
               ${lib.optionalString unshare-cgroup "--unshare-cgroup"} \
               -- ${drv}/bin/${name} ${lib.concatStringsSep " " args} "$@"
        ''
      )
  )
