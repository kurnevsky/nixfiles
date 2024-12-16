users: value:

{
  home-manager.users = builtins.listToAttrs (
    map (user: {
      name = user;
      inherit value;
    }) users
  );
}
