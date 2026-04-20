{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "anki-mcp-server";
  version = "0.15.1";

  src = fetchFromGitHub {
    owner = "ankimcp";
    repo = "anki-mcp-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-1xxHoSD/Li2f3ujfQEOmMro7IEkClZwCGX6cA1Bn8xQ=";
  };

  npmDepsHash = "sha256-XIihI386oRyenLGMROMhylOn/KMWnFQvQSLN21JoBuw=";

  meta = {
    description = "A Model Context Protocol (MCP) server that enables AI assistants to interact with Anki";
    homepage = "https://github.com/ankimcp/anki-mcp-server";
    license = lib.licenses.agpl3Plus;
  };
})
