{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "anki-mcp-server";
  version = "0.25.0";

  src = fetchFromGitHub {
    owner = "ankimcp";
    repo = "anki-mcp-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-LjAnk/IOjLsOebMOr9g4NUg7GpCmt/tF9ymO6dL9bdo=";
  };

  npmDepsHash = "sha256-BZ7qxRxkKxJsm59CgYAAzUGodmMceDPf58Tpu+XmSeY=";

  meta = {
    description = "A Model Context Protocol (MCP) server that enables AI assistants to interact with Anki";
    homepage = "https://github.com/ankimcp/anki-mcp-server";
    license = lib.licenses.agpl3Plus;
  };
})
