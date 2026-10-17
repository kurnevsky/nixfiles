# Plasma nested in the current compositor, in a bubblewrap sandbox running a
# separate guest NixOS system. Arguments run a different command in the same
# sandbox instead, e.g. `plasma-sandbox konsole`.
#
# `plasma-sandbox --user NAME [command...]` runs it as another user through su.
# That user reaches the parent compositor through a relay running as this
# user, which accepts only its connections, so it gets no access to this
# user's runtime directory.
#
# `--bind PATH` and `--ro-bind PATH` (repeatable) make PATH of the home
# directory (relative to it, or absolute inside it) available at the same place
# in the sandbox's home. With --user, it's the other user's home.
#
# Only the guest's closure is visible in /nix/store, /etc is the guest's one,
# and the user is `user` with a separate home directory from
# $PLASMA_SANDBOX_HOME (default ~/.local/share/plasma-sandbox/home).
# From the host the sandbox gets the parent compositor, PipeWire and
# PulseAudio sockets (per-user or system-wide ones) and the GPU. The system
# bus is a private one without services, as clients may refuse to start
# without any.
#
# The network namespace is a separate one connected by pasta (IPv4 only, with
# fixed addresses), so the host's interfaces, abstract sockets (X11 among
# others) and loopback aren't shared. Host loopback TCP ports listed in
# $PLASMA_SANDBOX_HOST_PORTS (e.g. 9050,9051) are reachable on the sandbox's
# loopback.
#
# With --torjail it runs in the torjail network namespace instead, entered
# through sudo as for other torjailed apps, with its DNS and the UTC timezone.
#
# bwrap always sets PR_SET_NO_NEW_PRIVS, so suid binaries don't work inside:
# sudo, pkexec and fusermount fail. The lock screen has a password of its own
# instead of the account's one, set with `plasma-sandbox --set-lock-password`
# (also with --user); until then it can't be unlocked.
{
  lib,
  writeShellApplication,
  writeShellScript,
  writeText,
  runCommandLocal,
  buildEnv,
  closureInfo,
  bubblewrap,
  coreutils,
  gnused,
  dbus,
  passt,
  openssl,
  writeCBin,
  # Evaluated guest NixOS system, see ./guest.nix
  guest,
}:

let
  inherit (guest) config;
  user = "user";
  home = "/home/${user}";
  etc = config.system.build.etc;
  drivers = buildEnv {
    name = "graphics-drivers";
    paths = [ config.hardware.graphics.package ] ++ config.hardware.graphics.extraPackages;
  };
  # KDE runs only desktop files that are owned by root, or that are inside its
  # standard directories after resolving symlinks. Inside, the store isn't
  # owned by root, as it's not mapped, so these are copies rather than links
  # into the store. The /etc ones take precedence, as they do normally.
  desktopFiles = runCommandLocal "plasma-sandbox-desktop-files" { } ''
    mkdir -p $out/share $out/etc/xdg/autostart
    cp -rL --no-preserve=mode ${config.system.path}/share/applications $out/share/
    for dir in ${config.system.path}/etc/xdg/autostart ${etc}/etc/xdg/autostart
    do
      if [ -d "$dir" ]
      then
        cp -rL --no-preserve=mode "$dir"/. $out/etc/xdg/autostart/
      fi
    done
  '';
  # Anything running inside is the same user, so everything is allowed.
  systemBusConfig = writeText "system-bus.conf" ''
    <!DOCTYPE busconfig PUBLIC "-//freedesktop//DTD D-Bus Bus Configuration 1.0//EN"
     "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
    <busconfig>
      <type>system</type>
      <listen>unix:path=/run/dbus/system_bus_socket</listen>
      <auth>EXTERNAL</auth>
      <policy context="default">
        <allow send_destination="*" eavesdrop="true"/>
        <allow eavesdrop="true"/>
        <allow own="*"/>
      </policy>
    </busconfig>
  '';
  init = writeShellScript "plasma-sandbox-init" ''
    . /etc/set-environment
    # Returns once the bus is listening. It's killed with the PID namespace.
    ${dbus}/bin/dbus-daemon --config-file=${systemBusConfig} --fork --nopidfile --nosyslog
    export XDG_DATA_DIRS="${desktopFiles}/share''${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"
    export XDG_CONFIG_DIRS="${desktopFiles}/etc/xdg''${XDG_CONFIG_DIRS:+:$XDG_CONFIG_DIRS}"
    if [ "$#" -eq 0 ]
    then
      set -- startplasma-wayland
    fi
    exec ${dbus}/bin/dbus-run-session "$@"
  '';
  # Address pasta forwards DNS queries from to the host's nameserver.
  dnsAddress = "169.254.1.1";
  resolvConf = writeText "resolv.conf" "nameserver ${dnsAddress}\n";
  unix-relay = writeCBin "unix-relay" (builtins.readFile ./unix-relay.c);
  cinfo = closureInfo {
    rootPaths = [
      etc
      config.system.path
      config.environment.binsh
      config.environment.usrbinenv
      drivers
      desktopFiles
      init
    ];
  };
in

writeShellApplication {
  name = "plasma-sandbox";
  runtimeInputs = [
    bubblewrap
    coreutils
    passt
    gnused
    openssl
  ];
  text = ''
    home_binds=()
    while [ "$#" -gt 0 ]
    do
      case "$1" in
        --user)
          target="''${2:?--user needs a user name}"
          shift 2
          ;;
        --torjail)
          torjail=1
          shift
          ;;
        --set-lock-password)
          set_lock_password=1
          shift
          ;;
        --bind|--ro-bind)
          home_binds+=("$1" "''${2:?$1 needs a path}")
          shift 2
          ;;
        --)
          shift
          break
          ;;
        *)
          break
          ;;
      esac
    done

    state="''${XDG_DATA_HOME:-$HOME/.local/share}/plasma-sandbox"

    # The hash the lock screen's password is checked against inside.
    if [ -n "''${set_lock_password-}" ]
    then
      if [ -n "''${target-}" ] && [ "$target" != "$(id -un)" ]
      then
        exec /run/wrappers/bin/su --login "$target" --command \
          "$(printf '%q ' "$(realpath "$0")" --set-lock-password)"
      fi
      read -r -s -p "Lock screen password: " password
      echo >&2
      read -r -s -p "Repeat: " repeated
      echo >&2
      if [ -z "$password" ] || [ "$password" != "$repeated" ]
      then
        echo "The passwords are empty or don't match" >&2
        exit 1
      fi
      mkdir -p "$state"
      umask 077
      printf '%s' "$password" | openssl passwd -6 -stdin > "$state/lock-password.new"
      mv "$state/lock-password.new" "$state/lock-password"
      exit
    fi

    : "''${WAYLAND_DISPLAY:?the session runs nested, so WAYLAND_DISPLAY must be set}"
    case "$WAYLAND_DISPLAY" in
      /*) wayland_socket="$WAYLAND_DISPLAY";;
      *) wayland_socket="''${XDG_RUNTIME_DIR:?XDG_RUNTIME_DIR must be set}/$WAYLAND_DISPLAY";;
    esac

    # Environment for running this again after su or sudo reset it.
    pass_vars() {
      vars=("$@")
      for var in PIPEWIRE_RUNTIME_DIR PULSE_SERVER PLASMA_SANDBOX_HOST_PORTS
      do
        if [ -n "''${!var-}" ]
        then
          vars+=("$var=''${!var}")
        fi
      done
    }

    # The other user reaches the parent compositor through a relay running as
    # this user and accepting only that user's connections. Its directory can
    # be traversed, but not listed.
    if [ -n "''${target-}" ] && [ "$target" != "$(id -un)" ]
    then
      relay_dir="$(mktemp -d)"
      relay_socket="$relay_dir/wayland"
      chmod 0711 "$relay_dir"
      ${unix-relay}/bin/unix-relay "$relay_socket" "$wayland_socket" "$(id -u "$target")" &
      relay_pid=$!
      trap 'kill "$relay_pid" 2>/dev/null; rm -rf "$relay_dir"' EXIT
      while [ ! -S "$relay_socket" ]
      do
        kill -0 "$relay_pid"
        sleep 0.01
      done

      pass_vars WAYLAND_DISPLAY="$relay_socket"
      /run/wrappers/bin/su --login "$target" --command \
        "$(printf '%q ' env "''${vars[@]}" "$(realpath "$0")" ''${torjail:+--torjail} "''${home_binds[@]}" -- "$@")"
      exit
    fi

    # The torjail network namespace can only be entered as root, through the
    # same sudo chain as for other torjailed apps, which ww is allowed to run.
    if [ -n "''${torjail-}" ] && [ -z "''${TORJAIL-}" ]
    then
      pass_vars WAYLAND_DISPLAY="$wayland_socket" TORJAIL=1
      for var in XDG_RUNTIME_DIR PLASMA_SANDBOX_HOME
      do
        if [ -n "''${!var-}" ]
        then
          vars+=("$var=''${!var}")
        fi
      done
      exec /run/wrappers/bin/sudo /run/current-system/sw/bin/ip netns exec torjail \
        sudo -u "$(id -un)" env "''${vars[@]}" "$(realpath "$0")" "''${home_binds[@]}" -- "$@"
    fi

    sandbox_home="''${PLASMA_SANDBOX_HOME:-$state/home}"
    mkdir -p "$state" "$sandbox_home"

    user_binds=()
    for ((i = 0; i < ''${#home_binds[@]}; i += 2))
    do
      path="''${home_binds[i + 1]}"
      case "$path" in
        "$HOME"/*) path="''${path#"$HOME"/}";;
        /*)
          echo "''${home_binds[i]} $path: not inside $HOME" >&2
          exit 2
          ;;
      esac
      case "/$path/" in
        */../*|*/./*|//)
          echo "''${home_binds[i]} $path: must be a plain path" >&2
          exit 2
          ;;
      esac
      if [ ! -e "$HOME/$path" ]
      then
        echo "''${home_binds[i]} $path: doesn't exist in $HOME" >&2
        exit 2
      fi
      user_binds+=("''${home_binds[i]}" "$HOME/$path" "${home}/$path")
    done

    uid="$(id -u)"
    gid="$(id -g)"
    runtime="/run/user/$uid"

    # Ids not mapped into the user namespace show up as the overflow ones.
    printf '%s\n' \
      'root:x:0:0::/var/empty:/run/current-system/sw/bin/nologin' \
      "${user}:x:$uid:$gid::${home}:/run/current-system/sw/bin/bash" \
      'nobody:x:65534:65534::/var/empty:/run/current-system/sw/bin/nologin' \
      > "$state/passwd"
    printf '%s\n' 'root:x:0:' "users:x:$gid:" 'nogroup:x:65534:' > "$state/group"

    if [ ! -s "$state/machine-id" ]
    then
      od -An -N16 -tx1 /dev/urandom | tr -d ' \n' > "$state/machine-id"
      echo >> "$state/machine-id"
    fi

    mapfile -t store_binds < <(sed 's/.*/--ro-bind\n&\n&/' ${cinfo}/store-paths)

    # /etc is a tmpfs populated with the guest's entries, so that the ones
    # coming from the host can be added.
    etc_binds=(--symlink ${etc}/etc /etc/static)
    for entry in ${etc}/etc/*
    do
      name="''${entry##*/}"
      case "$name" in
        # ownership and mode of copied entries, applied on activation
        *.mode|*.uid|*.gid) continue;;
      esac
      if [ -L "$entry" ]
      then
        etc_binds+=(--symlink "$(readlink "$entry")" "/etc/$name")
      else
        etc_binds+=(--ro-bind "$entry" "/etc/$name")
      fi
    done
    etc_binds+=(
      --ro-bind "$state/passwd" /etc/passwd
      --ro-bind "$state/group" /etc/group
      --ro-bind "$state/machine-id" /etc/machine-id
    )
    if [ -e "$state/lock-password" ]
    then
      etc_binds+=(--ro-bind "$state/lock-password" /etc/plasma-sandbox/lock-password)
    fi
    if [ ! -e ${etc}/etc/resolv.conf ]
    then
      if [ -n "''${TORJAIL-}" ]
      then
        etc_binds+=(--ro-bind /etc/resolv-torjail.conf /etc/resolv.conf)
      else
        etc_binds+=(--ro-bind ${resolvConf} /etc/resolv.conf)
      fi
    fi
    # The timezone name only, the guest has its own zoneinfo. Under torjail
    # it stays UTC, as for other torjailed apps.
    if [ -z "''${TORJAIL-}" ] && [ ! -L ${etc}/etc/localtime ] && [ -L /etc/localtime ]
    then
      timezone="$(readlink /etc/localtime)"
      etc_binds+=(--symlink "/etc/zoneinfo/''${timezone##*zoneinfo/}" /etc/localtime)
    fi

    # GPU nodes and only the sysfs and udev entries of their devices: libdrm
    # and mesa look the devices up in sysfs, kwin enumerates them with udev.
    gpu_binds=()
    udev_binds=()
    if [ -d /dev/dri ]
    then
      gpu_binds+=(--dev-bind /dev/dri /dev/dri)
      declare -A gpu_devices=()
      for node in /dev/dri/*
      do
        [ -c "$node" ] || continue
        devnum="$((0x$(stat -c %t "$node"))):$((0x$(stat -c %T "$node")))"
        char="/sys/dev/char/$devnum"
        class="/sys/class/drm/''${node##*/}"
        gpu_binds+=(
          --symlink "$(readlink "$char")" "$char"
          --symlink "$(readlink "$class")" "$class"
        )
        udev_binds+=(--ro-bind-try "/run/udev/data/c$devnum" "/run/udev/data/c$devnum")
        gpu_devices["$(realpath "$char/device")"]=1
      done
      for device in "''${!gpu_devices[@]}"
      do
        gpu_binds+=(
          --ro-bind "$device" "$device"
          --dir "$(realpath "$device/subsystem")"
        )
      done
    fi

    # Under a name compositors don't pick for themselves: the lock file the
    # parent holds isn't visible inside, so they would try to take it.
    runtime_binds=(--bind "$wayland_socket" "$runtime/wayland-host")
    # Per-user sound sockets.
    if [ -n "''${XDG_RUNTIME_DIR-}" ]
    then
      for x in pipewire-0 pipewire-0-manager pulse
      do
        if [ -e "$XDG_RUNTIME_DIR/$x" ]
        then
          runtime_binds+=(--bind "$XDG_RUNTIME_DIR/$x" "$runtime/$x")
        fi
      done
    fi

    # System-wide PipeWire and PulseAudio sockets, in the default locations
    # clients fall back to, or elsewhere as pointed to by the variables.
    sound_env=()
    for x in /run/pipewire /run/pulse
    do
      if [ -e "$x" ]
      then
        runtime_binds+=(--bind "$x" "$x")
      fi
    done
    if [ -n "''${PIPEWIRE_RUNTIME_DIR-}" ]
    then
      runtime_binds+=(--bind "$PIPEWIRE_RUNTIME_DIR" "$PIPEWIRE_RUNTIME_DIR")
      sound_env+=(--setenv PIPEWIRE_RUNTIME_DIR "$PIPEWIRE_RUNTIME_DIR")
    fi
    if [[ "''${PULSE_SERVER-}" == unix:/* ]]
    then
      pulse_dir="$(dirname "''${PULSE_SERVER#unix:}")"
      runtime_binds+=(--bind "$pulse_dir" "$pulse_dir")
      sound_env+=(--setenv PULSE_SERVER "$PULSE_SERVER")
    fi

    # Under torjail its network namespace is used as it is.
    net_args=()
    if [ -z "''${TORJAIL-}" ]
    then
      net_args+=(--unshare-net)
    elif [ -n "''${PLASMA_SANDBOX_HOST_PORTS-}" ]
    then
      echo "plasma-sandbox: PLASMA_SANDBOX_HOST_PORTS has no effect under torjail" >&2
    fi

    tmp="$(mktemp -d)"
    mkfifo "$tmp/info" "$tmp/block"

    bwrap_args=(
      "''${store_binds[@]}"
      --symlink ${config.environment.binsh} /bin/sh
      --symlink ${config.environment.usrbinenv} /usr/bin/env

      --tmpfs /etc
      "''${etc_binds[@]}"

      --proc /proc
      --dev /dev
      "''${gpu_binds[@]}"

      --tmpfs /run
      --symlink ${config.system.path} /run/current-system/sw
      --symlink ${drivers} /run/opengl-driver
      --dir /run/dbus
      "''${udev_binds[@]}"
      --symlink ../run /var/run
      --dir /var/tmp

      --perms 0700 --tmpfs "$runtime"
      "''${runtime_binds[@]}"

      --tmpfs /tmp
      --perms 1777 --dir /tmp/.X11-unix
      --bind "$sandbox_home" ${home}
      "''${user_binds[@]}"
      --chdir ${home}

      --clearenv
      --setenv HOME ${home}
      --setenv USER ${user}
      --setenv LOGNAME ${user}
      --setenv XDG_RUNTIME_DIR "$runtime"
      --setenv WAYLAND_DISPLAY wayland-host
      "''${sound_env[@]}"

      --unshare-user
      --unshare-ipc
      --unshare-pid
      --unshare-uts
      "''${net_args[@]}"
      --unshare-cgroup-try
      --hostname ${config.networking.hostName}

      --new-session
      --die-with-parent

      --cap-drop ALL

      --info-fd 5
      --block-fd 6
    )

    # Options are passed through a file descriptor, and bwrap gets no
    # environment: its init process, pid 1 in the sandbox, keeps both, and
    # they would expose host paths and the session's environment.
    #
    # The sandbox waits on the block fd until pasta has connected its network
    # namespace, which needs the pid bwrap reports on the info fd.
    env -i ${bubblewrap}/bin/bwrap --args 7 ${init} "$@" \
      5>"$tmp/info" 6<"$tmp/block" 7< <(printf '%s\0' "''${bwrap_args[@]}") &
    bwrap_pid=$!

    exec 5<"$tmp/info" 6>"$tmp/block"
    rm -r "$tmp"
    child_pid="$(sed -n 's/.*"child-pid": *\([0-9]*\).*/\1/p' <&5)"
    exec 5<&-
    if [ -z "$child_pid" ]
    then
      wait "$bwrap_pid"
      exit
    fi

    # Nothing is forwarded except DNS and the explicitly listed ports, and
    # the gateway address isn't mapped to the host's loopback either. The
    # interface gets a fixed name and addresses instead of the host's ones,
    # which pasta copies by default, and IPv4 only, so that the host's IPv6
    # address isn't exposed.
    if [ -z "''${TORJAIL-}" ]
    then
      pasta --quiet --config-net \
        --ipv4-only \
        --ns-ifname eth0 \
        --address 10.0.2.15 --netmask 24 --gateway 10.0.2.2 \
        --tcp-ports none --udp-ports none \
        --tcp-ns "''${PLASMA_SANDBOX_HOST_PORTS:-none}" --udp-ns none \
        --no-map-gw \
        --dns-forward ${dnsAddress} \
        "$child_pid"
    fi

    echo >&6
    exec 6>&-
    wait "$bwrap_pid"
  '';
}
