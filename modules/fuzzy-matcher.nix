{ symlinkJoin, melpaBuild, fetchFromGitHub, rustPlatform, writeText, emacs }:

let
  version = "0.0.1";
  pname = "fuzzy-matcher";
in rustPlatform.buildRustPackage {
  inherit pname version;

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "${pname}-el";
    rev = "ab33fac20f7ad365458ea32975b878cdb57ce4da";
    sha256 = "sha256-Oaqn2eKNKjuOMPsPhyLFdEqCLI8BV9j3UaSz556992w=";
  };

  nativeBuildInputs = [ rustPlatform.bindgenHook ];

  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    mv $out/lib/libfuzzy_matcher_el.so $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    rm -r $out/lib/
  '';

  cargoSha256 = "sha256-1B1zMshouFx2szaFD5LrrKRTb2uho0MLsCIbEqx/Pjk=";
}
