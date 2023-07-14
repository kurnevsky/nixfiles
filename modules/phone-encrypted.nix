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
      --bind ${pkgs.cryptsetup} ${pkgs.cryptsetup} \
      --tmpfs /run/cryptsetup \
      --dev-bind /dev/random /dev/random \
      ${pkgs.bash}/bin/bash -c '
        echo ${
          builtins.toJSON passphrase
        } | ${pkgs.cryptsetup}/bin/cryptsetup reencrypt \
          --encrypt /tmp/encrypted.img \
          --reduce-device-size $((slack*1024*1024))

        cryptsetup luksUUID --uuid=${builtins.toJSON uuid} /tmp/encrypted.img
      '

    mv /tmp/encrypted.img $out/
  '';
in {
  boot.initrd.luks.devices.LUKS-MOBILE-ROOTFS.device =
    "/dev/disk/by-uuid/${uuid}";

  fileSystems."/" = {
    device = "/dev/mapper/LUKS-MOBILE-ROOTFS";
    fsType = "ext4";
  };

  mobile.generatedFilesystems.rootfs.raw = encryptedRootfs;
}
