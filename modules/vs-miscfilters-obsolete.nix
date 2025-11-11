{
  stdenv,
  fetchFromGitHub,
  pkg-config,
  meson,
  ninja,
  cmake,
  vapoursynth,
  ...
}:

stdenv.mkDerivation rec {
  pname = "vs-miscfilters-obsolete";
  version = "2";

  src = fetchFromGitHub {
    owner = "vapoursynth";
    repo = pname;
    rev = "07e0589a381f7deb3bf533bb459a94482bccc5c7";
    hash = "sha256-WEhpBTNEamNfrNXZxtpTGsOclPMRu+yBzNJmDnU0wzQ=";
  };

  nativeBuildInputs = [
    pkg-config
    meson
    ninja
    cmake
  ];

  buildInputs = [
    vapoursynth
  ];

  postPatch = ''
    substituteInPlace meson.build \
      --replace-fail "dep.get_pkgconfig_variable('libdir')" "get_option('libdir')"
  '';
}
