{ pkgs, ... }:

let
  python = pkgs.python3.withPackages (pkgs: with pkgs; [ fonttools ]);
  font-freezer = pkgs.fetchFromGitHub {
    owner = "twardoch";
    repo = "fonttools-opentype-feature-freezer";
    rev = "2ae16853bc724c3e377726f81d9fc661d3445827";
    sha256 = "sha256-mIWQF9LTVKxIkwHLCTVK1cOuiaduJyX8pyBZ/0RKIVE=";
  };
  font = pkgs.nerdfonts.override {
    fonts = [ "JetBrainsMono" "Hack" "NerdFontsSymbolsOnly" ];
  };
in {
  fonts.packages = [
    (pkgs.symlinkJoin {
      name = font.name;
      paths = [ font ];
      postBuild = ''
        export PYTHONPATH=:"${font-freezer}/src/:''${PYTHONPATH}"
        find ''${out}/share/fonts/truetype/NerdFonts/ -name 'JetBrains*' -exec \
          ${python}/bin/python ${font-freezer}/src/opentype_feature_freezer/cli.py -f 'cv12,zero' {} \;
      '';
    })
  ];
}
