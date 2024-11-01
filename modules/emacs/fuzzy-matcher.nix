{ rustPlatform }:

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

  cargoHash = "sha256-YQf85rK4tIVOB10aNpFEZJvLDEPSrr9b/9steMM7ITU=";
}
