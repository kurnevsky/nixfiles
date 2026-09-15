{
  lib,
  python3Packages,
  fetchFromGitHub,
}:

let
  markdown-to-confluence = python3Packages.buildPythonPackage (finalAttrs: {
    pname = "markdown-to-confluence";
    version = "0.6.3";
    pyproject = true;

    src = fetchFromGitHub {
      owner = "hunyadi";
      repo = "md2conf";
      tag = finalAttrs.version;
      hash = "sha256-ejMJVU+C8KjVqpFnjJo5vIVB5J9wDqK8NVYCytGYd48=";
    };

    build-system = with python3Packages; [ setuptools ];

    # nixpkgs has a slightly older version that works fine
    pythonRelaxDeps = [ "orjson" ];

    dependencies = with python3Packages; [
      cattrs
      lxml
      markdown
      orjson
      pathspec
      pymdown-extensions
      pyyaml
      requests
      truststore
    ];

    pythonImportsCheck = [ "md2conf" ];

    meta = {
      description = "Publish Markdown files to Confluence wiki";
      homepage = "https://github.com/hunyadi/md2conf";
      license = lib.licenses.mit;
      mainProgram = "md2conf";
    };
  });
in

python3Packages.buildPythonApplication (finalAttrs: {
  pname = "mcp-atlassian";
  version = "0.23.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "sooperset";
    repo = "mcp-atlassian";
    tag = "v${finalAttrs.version}";
    hash = "sha256-zbBsEBEfSvsS7dTbbvx2HG2GQhxGtvwVpYf4JPZVCKw=";
  };

  # it's determined from the git history that is absent in the tarball
  env.UV_DYNAMIC_VERSIONING_BYPASS = finalAttrs.version;

  build-system = with python3Packages; [
    hatchling
    uv-dynamic-versioning
  ];

  # type stubs that aren't needed at runtime
  pythonRemoveDeps = [
    "types-cachetools"
    "types-python-dateutil"
  ];

  pythonRelaxDeps = [ "fakeredis" ];

  dependencies =
    with python3Packages;
    [
      anyio
      atlassian-python-api
      beautifulsoup4
      cachetools
      click
      fakeredis
      fastmcp
      httpx
      markdown
      markdownify
      mcp
      pydantic
      pysocks
      python-dateutil
      python-dotenv
      requests
      starlette
      thefuzz
      trio
      truststore
      unidecode
      urllib3
      uvicorn
    ]
    ++ [ markdown-to-confluence ];

  pythonImportsCheck = [ "mcp_atlassian" ];

  meta = {
    description = "MCP server for Atlassian products (Confluence and Jira)";
    homepage = "https://github.com/sooperset/mcp-atlassian";
    license = lib.licenses.mit;
    mainProgram = "mcp-atlassian";
  };
})
