{ rustPlatform }:

let
  version = "0.0.1";
  pname = "fuzzy-matcher";
in
rustPlatform.buildRustPackage {
  inherit pname version;

  src = ./fuzzy-matcher;

  nativeBuildInputs = [ rustPlatform.bindgenHook ];

  postInstall = ''
    mkdir -p $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    mv $out/lib/libfuzzy_matcher_el.so $out/share/emacs/site-lisp/elpa/${pname}-${version}/
    rm -r $out/lib/
  '';

  cargoLock = {
    lockFile = ./fuzzy-matcher/Cargo.lock;
    outputHashes = {
      "emacs-0.19.0" = "sha256-094GLODTvUpwjs77KTsAYBdytX726c2aNkyOS9sxRuU=";
    };
  };
}
