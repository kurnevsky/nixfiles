{ lib, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    displayManager.sddm.enable = true;
    desktopManager.plasma5.enable = true;
  };

  home-manager.users = let
    home = { lib, ... }:
      let
        configs = {
          kwinrc = {
            Compositing = {
              GLCore = true;
              OpenGLIsUnsafe = false;
            };
            Desktops = {
              Id_1 = "0f58e871-15cd-41d9-b264-f32e8cf4efc0";
              Id_2 = "ae53fcaa-bdb2-4c4a-a2f8-56b856436d4f";
              Id_3 = "3f087407-bee1-4ffd-8406-93daa6d6c8ef";
              Id_4 = "173d1149-fb35-457f-b5ad-83565b76af17";
              Id_5 = "beac7049-1de6-4e72-867b-5d022ecdabb6";
              Id_6 = "51980c0a-2ca4-420c-89d6-9c44793fff9f";
              Id_7 = "e15b8d6e-ca11-4f48-ab7c-21bee08d196b";
              Id_8 = "9a84dcbd-836e-4fa0-a8b8-d13132fbd2dd";
              Id_9 = "430241ed-707e-442a-9836-84702bd2bde0";
              Number = 9;
              Rows = 3;
            };
            Effect-Slide.Duration = 300;
            Plugins.invertEnabled = true;
            TabBox.LayoutName = "thumbnail_grid";
            Windows.RollOverDesktops = false;
          };
        };
        lines = lib.flatten (lib.mapAttrsToList (file: groups:
          lib.mapAttrsToList (group: keys:
            lib.mapAttrsToList (key: value:
              "${pkgs.libsForQt5.kconfig}/bin/kwriteconfig5 --file ~/.config/${file} --group ${group} --key ${key} ${
                builtins.toJSON value
              }") keys) groups) configs);
      in {
        home.activation.kdeConfigs = lib.hm.dag.entryAfter [ "writeBoundary" ]
          (builtins.concatStringsSep "\n" lines);
      };
  in {
    kurnevsky = home;
    ww = home;
  };
}
