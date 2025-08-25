{
  lib,
  ...
}:

{
  users.users.kurnevsky = {
    hashedPasswordFile = lib.mkForce null;
    password = "1234";
  };

  nixpkgs.overlays = [
    (_self: super: {
      pinenote-dbus-service = super.pinenote-dbus-service.overrideAttrs (old: {
        postPatch = ''
          substituteInPlace src/ebc_ioctl.rs \
            --replace-fail 'ptr_screen_content: *const u8,' 'ptr_screen_content: *const i8,'
        '';
      });
    })
  ];
}
