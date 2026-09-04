{
  lib,
  fetchFromGitHub,
  buildGoModule,
}:

buildGoModule (finalAttrs: {
  pname = "miniflux-mcp";
  version = "0.5.0";

  src = fetchFromGitHub {
    owner = "tssujt";
    repo = "miniflux-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-9sW+xEVuy22wga4G7WzwjIUglMh74bjAGwWrqCHRFIs=";
  };

  vendorHash = "sha256-KQiLj/XceaqrJocFavqCxYjPg2pkm5vjfy6hIjfz6Ac=";

  ldflags = [
    "-X main.Version=${finalAttrs.version}"
  ];

  meta = {
    description = "A Model Context Protocol (MCP) server for interacting with Miniflux RSS reader";
    homepage = "https://github.com/tssujt/miniflux-mcp";
    license = lib.licenses.mit;
    mainProgram = "miniflux-mcp";
  };
})
