{ symlinkJoin, melpaBuild, rustPlatform, writeText, emacs }:

let
  version = "0.0.1";
  pname = "fuzzy-matcher";
in rustPlatform.buildRustPackage {
  inherit pname version;

  src = ./fuzzy-matcher;

  nativeBuildInputs = [ rustPlatform.bindgenHook ];

  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    mv $out/lib/libfuzzy_matcher_el.so $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    rm -r $out/lib/
  '';

  cargoSha256 = "sha256-4NzX+2iHrxYIRTq+/szZkwCbSSBCV10kht9Pn6br8uA=";
}
