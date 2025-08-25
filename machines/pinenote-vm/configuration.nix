{
  lib,
  ...
}:

{
  users.users.kurnevsky = {
    hashedPasswordFile = lib.mkForce null;
    password = "1234";
  };
}
