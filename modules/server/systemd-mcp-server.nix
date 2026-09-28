{
  lib,
  fetchFromGitHub,
  buildGoModule,
  makeWrapper,
  systemd,
}:

buildGoModule (finalAttrs: {
  pname = "systemd-mcp";
  version = "0.3.5";

  src = fetchFromGitHub {
    owner = "openSUSE";
    repo = "systemd-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-geulShMhqLJgDaW8b4EFovIj8lZIUUpdYAdD5vlREnY=";
  };

  vendorHash = "sha256-g+HJztorAsCdBVXwjTfTSXKsul027pPMJ/khMWkBPuA=";

  subPackages = [
    "."
    "gatekeeper"
  ];

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [ systemd ];

  postInstall = ''
    install -D -m 0644 configs/com.suse.gatekeeper.policy -t $out/share/polkit-1/actions
  '';

  postFixup = ''
    # libsystemd is opened with dlopen, so it's not in the rpath
    wrapProgram $out/bin/systemd-mcp \
      --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ systemd ]}
  '';

  meta = {
    description = "MCP server for systemd";
    homepage = "https://github.com/openSUSE/systemd-mcp";
    license = lib.licenses.mit;
    mainProgram = "systemd-mcp";
  };
})
