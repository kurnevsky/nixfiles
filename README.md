<!-- markdownlint-configure-file
{ "line-length": { "line_length": 200 } }
-->

# NixFiles

My personal [NixOS](https://nixos.org/) configuration.

## Nix commands

| Command                                                                                             | Description                                       |
|-----------------------------------------------------------------------------------------------------|---------------------------------------------------|
| `nixos-rebuild switch --keep-going -L`                                                              | apply configuration                               |
| `nixos-rebuild switch --keep-going --option substitute false -L`                                    | apply configuration offline                       |
| `nixos-rebuild switch --keep-going -L --option extra-substituters 'ssh://somehost'`                 | apply configuration using ssh store               |
| `nixos-rebuild switch --keep-going -L --option builders 'ssh://somehost - - N' --option max-jobs 0` | apply configuration using ssh builder with N jobs |
| `nixos-rebuild switch --keep-going --target-host 'ssh://somehost' --sudo -L`                        | apply configuration to a remote host              |
| `all_proxy=socks5://127.0.0.1:1080 nixos-rebuild switch -L`                                         | apply configuration using proxy                   |
| `nix-collect-garbage --delete-old`                                                                  | collect garbage                                   |

To use `nix-output-monitor` add `--log-format internal-json |& nom --json` at the end of your command.

### Nix-on-droid commands

Specifying `binfmt.emulatedSystems` allows to build packages remotely for a different architecture using qemu. Nix-on-droid can be called like:

`nix-on-droid switch --flake ~/.config/nixfiles#default --max-jobs 0 --builders 'ssh://nix-ssh@somehost x86_64-linux,aarch64-linux - N - big-parallel'`

## Sandboxing

Some applications are sandboxed using [bubblewrap](https://github.com/containers/bubblewrap). See [sandbox](modules/sandbox.nix) module for details.

The following environment variables are supported:

| Variable     | Description                                                                                                                          |
| ------------ | ------------------------------------------------------------------------------------------------------------------------------------ |
| BLACKLIST    | additional blacklisted paths                                                                                                         |
| CAMERA       | a newline separated list of `/dev/video*` devices that will be allowed (useful for applications that don't allow to choose a camera) |
| DNS          | override DNS server (useful in case of running an application inside network namespace that don't have access to localhost)          |
| NOLOCALTIME  | use UTC timezone (if not set will be inherited from TORJAIL)                                                                         |
| RO_WHITELIST | additional readonly whitelisted paths                                                                                                |
| TORJAIL      | indicate that an application is running inside tor jail (necessary for proper DNS resolution)                                        |
| UNSANDBOXED  | run application without sandbox                                                                                                      |
| WHITELIST    | additional whitelisted paths                                                                                                         |
| WITH_NETWORK | allow network access (if it's disabled by default)                                                                                   |

## Breaking updates

In case something breaks during update an old version can be used with an overlay like:

```nix
{ pkgs, ... }:

let oldPkgs = import inputs.nixpkgs-old {
  inherit (pkgs.stdenv.targetPlatform) system;
}; in {
  nixpkgs.overlays = [
    (_self: _super: {
      inherit (oldPkgs) some-broken-package;
    })
  ];
}
```

A patch to a derivation can be applied like:

```nix
{ pkgs, ... }:

let
  patchesDrv = pkgs.applyPatches {
    src = pkgs.path;
    patches = [
      (pkgs.fetchpatch {
        url = "https://some-patch.diff";
        hash = "";
      })
    ];
  };
  patchedPkgs = import patchesDrv { inherit (pkgs.stdenv.targetPlatform) system; };
in {
  nixpkgs.overlays = [ (_self: _super: { inherit (patchedPkgs) some-broken-package; }) ];
}
```

A module can be patched like:

```nix
{ pkgs, ... }:

{
  disabledModules = [ "services/networking/some-broken-module.nix" ];
  imports = [
    (builtins.fetchurl {
      url = "https://some-fixed-module.nix";
      hash = "";
    })
  ];
}
```

## License

Licensed under [GPLv3+](/LICENSE) with an exception that allows code from this repository to be incorporated into
projects that are used as flake inputs by this project, regardless of their license.
