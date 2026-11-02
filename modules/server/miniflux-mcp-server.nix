{
  lib,
  fetchFromGitHub,
  buildGo127Module,
}:

buildGo127Module (finalAttrs: {
  pname = "miniflux-mcp";
  version = "0.8.0";

  src = fetchFromGitHub {
    owner = "tssujt";
    repo = "miniflux-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-NppZpVXB2ButlHBeMY1JsyRUrwlBrhaPw2pQbtHrej0=";
  };

  vendorHash = "sha256-ZcIT0tI7akGYeVQYxQrbd0PXY2lEeDMkESLjbZg3g9Q=";

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
