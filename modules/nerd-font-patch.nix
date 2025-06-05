{
  nerd-font-patcher,
  parallel,
  stdenv,
}:

font:

stdenv.mkDerivation {
  name = "${font.name}-nerd-font-patched";
  src = font;
  buildPhase = ''
    find \( -name \*.ttf -o -name \*.otf \) -print0 | \
      ${parallel}/bin/parallel -j ''${NIX_BUILD_CORES:0} -u -0 ${nerd-font-patcher}/bin/nerd-font-patcher -c {}
  '';
  installPhase = "cp -a . $out";
}
