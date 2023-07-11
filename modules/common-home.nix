users:

{ ... }:

{
  home-manager.users = builtins.listToAttrs (map (user: {
    name = user;
    value = ./common-home-config.nix;
  }) users);
}
