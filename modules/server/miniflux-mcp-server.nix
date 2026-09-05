{
  lib,
  fetchFromGitHub,
  buildGoModule,
}:

buildGoModule (finalAttrs: {
  pname = "miniflux-mcp";
  version = "0.6.0";

  src = fetchFromGitHub {
    owner = "tssujt";
    repo = "miniflux-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-82/jE3wa1Kgg0S0DpqLWdBadIrp30HJmmvPoGgQpMc0=";
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
