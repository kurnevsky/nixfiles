{ lib, pkgs, rootfs, ... }:

let
  passphrase = "1234";
  uuid = "da6921e8-9c36-4216-8fbc-fa781e2a0fb7";
  encryptedRootfs = pkgs.runCommand "encrypted-rootfs" {
    passthru = {
      filename = "encrypted.img";
      filesystemType = "LUKS";
    };
  } ''
    mkdir -p $out

    export slack=32 # MiB

    # Some slack space we'll append to the raw fs
    # Used by `--reduce-device-size` read cryptsetup(8).
    dd if=/dev/zero of=/tmp/slack.img bs=1024 count=$((slack*1024))

    # Catting both to ensure it's writable, and to add some slack space at
    # the end
    cat ${rootfs}/${rootfs.label}.img /tmp/slack.img > /tmp/encrypted.img
    rm /tmp/slack.img

    ${pkgs.bubblewrap}/bin/bwrap \
      --ro-bind /nix/store /nix/store \
      --dev-bind /dev/random /dev/random \
      --dev-bind /dev/urandom /dev/urandom \
      --tmpfs /run/cryptsetup \
      --bind /tmp/encrypted.img /tmp/encrypted.img \
      ${pkgs.bash}/bin/bash -c '
        ${pkgs.coreutils}/bin/echo ${
          builtins.toJSON passphrase
        } | ${pkgs.cryptsetup}/bin/cryptsetup reencrypt \
          --encrypt /tmp/encrypted.img \
          --reduce-device-size $((slack*1024*1024))

        ${pkgs.cryptsetup}/bin/cryptsetup luksUUID \
          --uuid=${builtins.toJSON uuid} \
          /tmp/encrypted.img
      '

    mv /tmp/encrypted.img $out/
  '';
in {
  # breaks encryption
  mobile.quirks.supportsStage-0 = lib.mkForce false;

  boot.initrd.luks.devices.root-luks = {
    allowDiscards = true;
    device = "/dev/disk/by-uuid/${uuid}";
  };

  fileSystems."/" = {
    device = "/dev/mapper/root-luks";
    fsType = "ext4";
  };

  mobile.generatedFilesystems.rootfs.raw = encryptedRootfs;
}
