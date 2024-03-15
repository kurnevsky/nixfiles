{ fontforge-fonttools, nerd-font-patcher, stdenv }:

font:

stdenv.mkDerivation {
  name = "${font.name}-nerd-font-patched";
  src = font;
  buildPhase = ''
    find \( -name \*.ttf -o -name \*.otf \) -execdir ${nerd-font-patcher}/bin/nerd-font-patcher -c {} \;
  '';
  installPhase = "cp -a . $out";
}
