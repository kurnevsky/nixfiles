{ nerd-font-patcher, stdenv }:

font:

stdenv.mkDerivation {
  name = "${font.name}-nerd-font-patched";
  src = font;
  nativeBuildInputs = [ nerd-font-patcher ];
  buildPhase = ''
    find -name \*.ttf -o -name \*.otf -exec nerd-font-patcher -c {} \;
  '';
  installPhase = "cp -a . $out";
}
