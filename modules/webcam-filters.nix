{ lib, fetchFromGitHub, python3, python3Packages, poetry2nix, gst_all_1
, wrapGAppsHook, gobject-introspection, cmake }:

poetry2nix.mkPoetryApplication rec {
  pname = "webcam-filters";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "jashandeep-sohi";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-VifL79jvD4hqFKdyi/SCusWmFrO7BXMpD9Cnyp06QmI=";
  };

  python = python3;
  projectDir = src;

  # https://github.com/NixOS/nixpkgs/issues/56943
  strictDeps = false;

  # https://github.com/nix-community/poetry2nix/issues/155
  # TODO: upstreamed, remove after next release
  overrides = poetry2nix.overrides.withDefaults (self: super: {
    opencv-contrib-python = super.opencv-contrib-python.overridePythonAttrs
      (old: {
        nativeBuildInputs = [ cmake ] ++ old.nativeBuildInputs;
        buildInputs = [ self.scikit-build ] ++ (old.buildInputs or [ ]);
        dontUseCmakeConfigure = true;
      });
  });

  nativeBuildInputs = [ wrapGAppsHook gobject-introspection ];

  buildInputs = [
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-vaapi
  ];

  propagatedBuildInputs = with python3Packages; [ gst-python pygobject3 ];

  meta = with lib; {
    description =
      "Adds filters (background blur, etc) to your webcam on Linux.";
    homepage = "https://github.com/jashandeep-sohi/webcam-filters";
    license = licenses.gpl3Only;
    platforms = platforms.linux;
  };
}
