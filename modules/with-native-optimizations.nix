drv:

drv.overrideAttrs (old: {
  pname = old.pname + "-unsafe";
  NIX_CFLAGS_COMPILE = [ "-O3" "-march=native" "-mtune=native" ];
  NIX_ENFORCE_NO_NATIVE = false;
  preferLocalBuild = true;
  allowSubstitutes = false;
})
