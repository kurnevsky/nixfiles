{
  user =
    let
      id = 10162;
    in
    {
      uid = id;
      gid = id;
    };

  system.stateVersion = "23.05";
  home-manager.config.home.stateVersion = "23.05";
}
