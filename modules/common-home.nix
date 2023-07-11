users:

{ ... }:

{
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = builtins.listToAttrs (map (user: {
      name = user;
      value = ./common-home-config.nix;
    }) users);
  };
}
