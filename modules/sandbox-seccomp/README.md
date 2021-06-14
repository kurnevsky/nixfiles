Install dependencies (ArchLinux):
```sh
pacman -S libseccomp
```

Install dependencies (Ubuntu):
```sh
apt install libseccomp-dev
```

On NixOS execute:
```sh
nix-shell -p gcc libseccomp
```

Compile:
```sh
gcc seccomp-gen.c -lseccomp -Wall -pedantic -o seccomp-gen
```

Run:
```sh
./seccomp-gen
```

At last step, a file named `seccomp.bpf` will be generated.
This seccomp filter can then be used in other apps (e.g. bubblewrap).
