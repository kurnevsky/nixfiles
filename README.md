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
| `nixos-rebuild switch --upgrade --recreate-lock-file --keep-going -L`                               | apply configuration and update dependencies       |
| `nixos-rebuild switch --keep-going -L --option extra-substituters 'ssh://somehost'`                 | apply configuration using ssh store               |
| `nixos-rebuild switch --keep-going -L --option builders 'ssh://somehost - - N' --option max-jobs 0` | apply configuration using ssh builder with N jobs |
| `all_proxy=socks5://127.0.0.1:1080 nixos-rebuild switch -L`                                         | apply configuration using proxy                   |
| `nix-collect-garbage --delete-old`                                                                  | collect garbage                                   |

### Nix-on-droid commands

Specifying `binfmt.emulatedSystems` allows to build packages remotely for a different architecture using qemu. Nix-on-droid can be called like:

`nix-on-droid switch --flake ~/.config/nixfiles#default --max-jobs 0 --builders 'ssh://nix-ssh@somehost - - N'`

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
