{ stdenv, libseccomp }:

stdenv.mkDerivation {
  name = "sandbox-seccomp";
  src = ./seccomp;
  buildInputs = [ libseccomp ];
  buildPhase =
    "gcc seccomp-gen.c -lseccomp -Wall -pedantic -o seccomp-gen && ./seccomp-gen";
  installPhase = "mkdir -p $out && cp seccomp.bpf $out";
}
