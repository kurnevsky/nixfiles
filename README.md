My personal [NixOS](https://nixos.org/) configuration.

# Sandboxing

Some applications are sandboxed using [bubblewrap](https://github.com/containers/bubblewrap). See [sandbox](modules/sandbox.nix) module for details.

The following environment variables are supported:

| Variable     | Description                                                                                                                          |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------|
| BLACKLIST    | additional blacklisted paths                                                                                                         |
| CAMERA       | a newline separated list of `/dev/video*` devices that will be allowed (useful for applications that don't allow to choose a camera) |
| DNS          | override DNS server (useful in case of running an application inside network namespace that don't have access to localhost)          |
| NOLOCALTIME  | use UTC timezone (if not set will be inherited from TORJAIL)                                                                         |
| RO_WHITELIST | additional readonly whitelisted paths                                                                                                |
| TORJAIL      | indicate that an application is running inside tor jail (necessary for proper DNS resolution)                                        |
| UNSANDBOXED  | run application without sandbox                                                                                                      |
| WHITELIST    | additional whitelisted paths                                                                                                         |
| WITH_NETWORK | allow network access (if it's disabled by default)                                                                                   |
