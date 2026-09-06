{
  lib,
  fetchFromGitHub,
  buildGoModule,
}:

buildGoModule (finalAttrs: {
  pname = "prometheus-mcp-server";
  version = "0.18.0";

  src = fetchFromGitHub {
    owner = "prometheus";
    repo = "prometheus-mcp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-67pkE28hbORN+etTkon4u9HzWTKL0K1q0GNp8X2Oaqo=";
    # the prometheus documentation is embedded into the binary
    fetchSubmodules = true;
  };

  vendorHash = "sha256-QAny2SDYHaUHgiUn0KNesDmwfwh5NWpUB3FtwsB2g/I=";

  subPackages = [ "cmd/prometheus-mcp-server" ];

  ldflags = [
    "-X github.com/tjhop/prometheus-mcp-server/internal/version.Version=${finalAttrs.version}"
  ];

  meta = {
    description = "MCP server to allow LLMs to interact with a running Prometheus instance";
    homepage = "https://github.com/prometheus/prometheus-mcp";
    license = lib.licenses.asl20;
    mainProgram = "prometheus-mcp-server";
  };
})
