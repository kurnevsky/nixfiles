{ lib, iosevka, ... }:

variant: ligations:

(iosevka.override {
  set = variant;
  privateBuildPlan = {
    family = "Iosevka ${variant}";
    leading = 1000;
    spacing = lib.toLower variant;
    serifs = "sans";
    noCvSs = true;
    exportGlyphNames = false;
    noLigation = !ligations;
    variants = {
      inherits = "ss14";
      design = {
        capital-g = "toothed-serifless-hooked";
        capital-j = "serifed";
        capital-q = "crossing";
        u = "toothed-serifless";
        capital-u = "toothed-serifless";
        w = "straight-serifless";
        capital-w = "straight-serifless";
        zero = "slashed";
        brace = "straight";
        question = "smooth";
        paren = "normal";
      };
    };
    widths = {
      Condensed = {
        shape = 500;
        menu = 3;
        css = "condensed";
      };
      Normal = {
        shape = 600;
        menu = 5;
        css = "normal";
      };
      Extended = {
        shape = 720;
        menu = 7;
        css = "expanded";
      };
    };
    ligations.inherits = "dlig";
  };
})
