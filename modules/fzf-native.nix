{ fetchFromGitHub, melpaBuild, writeText, cmake }:

let
  rev = "ba06f5149e23c57510de2e67d5c46aee96356dba";
  owner = "dangduc";
  pname = "fzf-native";
  version = "1";
in melpaBuild {
  inherit pname version;

  commit = rev;

  src = fetchFromGitHub {
    inherit owner rev;
    repo = pname;
    sha256 = "sha256-nRQTfkse+9icwJ4DEleiGbeIHEGqAMw97qk4jukdneE=";
  };

  nativeBuildInputs = [ cmake ];

  postInstall = ''
    install -m=755 -D source/bin/Linux/fzf-native-module.so $out/share/emacs/site-lisp/elpa/${pname}-${version}/bin/Linux/fzf-native-module.so
  '';

  recipe = writeText "recipe" ''
    (${pname} :repo "${owner}/${pname}" :fetcher github)
  '';
}
