{ lib, stdenv, libseccomp }:

blacklist:

stdenv.mkDerivation {
  name = "sandbox-seccomp";
  src = ./seccomp;
  buildInputs = [ libseccomp ];
  postPatch = "substituteInPlace seccomp-gen.c --subst-var-by rules '${
      lib.concatMapStrings (call: "DENY_RULE(${call});") blacklist
    }'";
  buildPhase =
    "gcc seccomp-gen.c -lseccomp -Wall -pedantic -o seccomp-gen && ./seccomp-gen";
  installPhase = "mkdir -p $out && cp seccomp.bpf $out";
}
