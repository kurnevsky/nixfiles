hostname: drv:

drv.overrideAttrs (old: {
  pname = old.pname + "-" + hostname + "-unsafe";
  # Keep this a plain string: with __structuredAttrs a list-valued
  # NIX_CFLAGS_COMPILE becomes a bash array that is never exported to the
  # compiler, which also drops the -isystem flags the cc-wrapper appends for
  # buildInputs (e.g. spirv-headers for llama-cpp with vulkan).
  NIX_CFLAGS_COMPILE = toString (old.NIX_CFLAGS_COMPILE or "") + " -O3 -march=native -mtune=native";
  NIX_ENFORCE_NO_NATIVE = false;
  preferLocalBuild = true;
  allowSubstitutes = false;
})
