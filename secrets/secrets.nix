# To edit:
# agenix --identity /etc/ssh/ssh_host_ed25519_key -e file.age

let
  evo =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP6yeYMiSNfRevRu+wO5JKyL5gt3CeHc6tAjdDgRSHW5";
  pc =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBfLzi6a22sanF3oeEheHjEjvvYRmJBUYFGXnj6NgSAJ";
  pinephone =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQ8G294HKMYY/cdc5dxMdJxHqTyA8jlZ7zTDrSK2sg2";
  digitalocean =
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMZRXGjEQLetY1TuKTIY/F08MLbVZs5QWjSMIe8PRIfU";
  desktop = [ evo pc ];
  all = desktop ++ [ pinephone digitalocean ];
in {
  "kurnevsky-evo.age".publicKeys = [ evo ];
  "ww-evo.age".publicKeys = [ evo ];
  "store-evo.age".publicKeys = [ evo ];

  "kurnevsky-pc.age".publicKeys = [ pc ];
  "ww-pc.age".publicKeys = [ pc ];
  "store-pc.age".publicKeys = [ pc ];

  "kurnevsky-digitalocean.age".publicKeys = [ digitalocean ];
  "store-digitalocean.age".publicKeys = [ digitalocean ];

  "tox.age".publicKeys = [ digitalocean ];

  "motion.age".publicKeys = desktop;

  "hans.age".publicKeys = all;
  "iodine.age".publicKeys = all;
  "shadowsocks.age".publicKeys = all;

  "wg-private-evo.age".publicKeys = [ evo ];
  "wg-preshared-evo.age".publicKeys = [ evo digitalocean ];

  "wg-private-pc.age".publicKeys = [ pc ];
  "wg-preshared-pc.age".publicKeys = [ pc digitalocean ];

  "github.age".publicKeys = all;
}
