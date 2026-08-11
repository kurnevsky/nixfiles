# Enter a Nix-on-Droid configuration in a bubblewrap sandbox, mimicking the
# proot environment the app sets up on a device: the activation package's
# filesystem is bound over /bin, /usr and /etc, the real activation script is
# run on first use (and whenever the generation changes), and login-inner
# execs the configured user shell.
{ pkgs, droid }:
let
  inherit (droid.config) user;
  inherit (droid.config.build) installationDir;
  act = droid.activationPackage;
in
pkgs.writeShellApplication {
  name = "droid-shell";
  runtimeInputs = with pkgs; [
    bubblewrap
    coreutils
  ];
  text = ''
    state="''${DROID_SHELL_STATE:-''${XDG_DATA_HOME:-$HOME/.local/share}/droid-shell}"
    mkdir -p "$state/home" "$state/etc" "$state/tmp" "$state/profiles/per-user/${user.userName}"

    if [ ! -e "$state/rootfs/bin/sh" ]; then
      mkdir -p "$state/rootfs"
      cp -RP --no-preserve=mode ${act}/filesystem/. "$state/rootfs/"
    fi

    # On a device /nix physically lives inside the installation directory, so
    # some activation scripts resolve store paths through it.
    mkdir -p "$state/instdir"
    ln -sfn /nix "$state/instdir/nix"

    sandbox() {
      bwrap \
        --bind /nix /nix \
        --bind "$state/profiles" /nix/var/nix/profiles \
        --ro-bind / /android \
        --proc /proc \
        --dev /dev \
        --bind "$state/etc" /etc \
        --bind "$state/rootfs/bin" /bin \
        --bind "$state/rootfs/usr" /usr \
        --bind "$state/tmp" /tmp \
        --bind "$state/home" ${user.home} \
        --bind "$state/instdir" ${installationDir} \
        --unshare-user \
        --uid ${toString user.uid} \
        --gid ${toString user.gid} \
        --clearenv \
        --setenv TERM "''${TERM:-xterm-256color}" \
        --setenv USER ${user.userName} \
        --setenv HOME ${user.home} \
        --setenv NIX_REMOTE daemon \
        --chdir ${user.home} \
        "$@"
    }

    generation=""
    if [ -L "$state/generation" ]; then
      generation="$(readlink "$state/generation")"
    fi
    if [ "$generation" != ${act} ]; then
      sandbox ${act}/activate
      ln -sfn ${act} "$state/generation"
    fi

    sandbox /bin/sh /usr/lib/login-inner "$@"
  '';
}
