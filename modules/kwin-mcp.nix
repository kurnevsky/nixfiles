{
  lib,
  python3Packages,
  fetchFromGitHub,
  gobject-introspection,
  wrapGAppsNoGuiHook,
  at-spi2-core,
  libei,
  wayland,
  dbus,
  wl-clipboard,
  wtype,
  wayland-utils,
}:

let
  dependencies = with python3Packages; [
    dbus-python
    mcp
    pillow
    pygobject3
  ];
in

# 0.10.0+ requires mcp 2.x which is not in nixpkgs yet
python3Packages.buildPythonApplication (finalAttrs: {
  pname = "kwin-mcp";
  version = "0.9.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "isac322";
    repo = "kwin-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-r5pX9pgioSwB1G7NDd4+yFoObseaPzq+/t10BmOkVIM=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace-fail 'uv_build>=0.10.3,<0.13.0' 'uv_build'
    substituteInPlace src/kwin_mcp/input.py \
      --replace-fail '"libei.so.1"' '"${lib.getLib libei}/lib/libei.so.1"'
    substituteInPlace src/kwin_mcp/clipboard.py \
      --replace-fail '"libwayland-client.so.0"' '"${lib.getLib wayland}/lib/libwayland-client.so.0"'
  '';

  build-system = with python3Packages; [ uv-build ];

  nativeBuildInputs = [
    gobject-introspection
    wrapGAppsNoGuiHook
  ];

  buildInputs = [ at-spi2-core ];

  inherit dependencies;

  dontWrapGApps = true;

  # helpers are spawned as `sys.executable -m kwin_mcp.<module>`, which
  # doesn't get the site packages of the wrapped script
  preFixup = ''
    makeWrapperArgs+=(
      "''${gappsWrapperArgs[@]}"
      --prefix PYTHONPATH : "$out/${python3Packages.python.sitePackages}:${python3Packages.makePythonPath dependencies}"
      --prefix PATH : ${
        lib.makeBinPath [
          dbus
          wl-clipboard
          wtype
          wayland-utils
        ]
      }
    )
  '';

  pythonImportsCheck = [ "kwin_mcp" ];

  meta = {
    description = "MCP server for Linux desktop GUI automation on KDE Plasma 6 Wayland";
    homepage = "https://github.com/isac322/kwin-mcp";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "kwin-mcp";
  };
})
