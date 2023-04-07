{ fetchFromGitHub, melpaBuild, writeText, cmake }:

let rev = "ba06f5149e23c57510de2e67d5c46aee96356dba";
in melpaBuild {
  pname = "fzf-native";
  version = "1";

  commit = rev;

  src = fetchFromGitHub {
    owner = "dangduc";
    repo = "fzf-native";
    inherit rev;
    sha256 = "sha256-nRQTfkse+9icwJ4DEleiGbeIHEGqAMw97qk4jukdneE=";
  };

  nativeBuildInputs = [ cmake ];

  postInstall = ''
    install -m=755 -D source/bin/Linux/fzf-native-module.so $out/share/emacs/site-lisp/elpa/fzf-native-1/bin/Linux/fzf-native-module.so
  '';

  recipe = writeText "recipe" ''
    (fzf-native :repo "dangduc/fzf-native" :fetcher github)
  '';
}
