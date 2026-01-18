# To edit:
# agenix --identity /etc/ssh/ssh_host_ed25519_key -e file.age
# To re-encrypt single secret use `EDITOR=:`

let
  evo = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIP6yeYMiSNfRevRu+wO5JKyL5gt3CeHc6tAjdDgRSHW5";
  pc = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBfLzi6a22sanF3oeEheHjEjvvYRmJBUYFGXnj6NgSAJ";
  pinephone = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQ8G294HKMYY/cdc5dxMdJxHqTyA8jlZ7zTDrSK2sg2";
  pinephone-pro = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJBYq3bQssdHLFzlMVy9MgiwFEbRr2BkWHhpC4I9H7B/";
  pinenote = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJrU/bW9L1mDvY7jfyoyP8YloGEaSYW4tpE6K8ggI8UV";
  vps = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN1EBLGp80pLSSm1s69+BT91TrmEN1LThcATLz4xR+6d";
  acer = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKi+GXCNLPnjSXaZaNAPEUF6Ve3ydpnXjyo3OZMzEHG0";
  desktop = [
    evo
    pc
  ];
  all = desktop ++ [
    acer
    pinephone
    pinephone-pro
    pinenote
    vps
  ];
in
{
  "kurnevsky-evo.age".publicKeys = [ evo ];
  "ww-evo.age".publicKeys = [ evo ];
  "store-evo.age".publicKeys = [ evo ];
  "syncthing-key-evo.age".publicKeys = [ evo ];
  "syncthing-cert-evo.age".publicKeys = [ evo ];

  "kurnevsky-pc.age".publicKeys = [ pc ];
  "ww-pc.age".publicKeys = [ pc ];
  "store-pc.age".publicKeys = [ pc ];
  "syncthing-key-pc.age".publicKeys = [ pc ];
  "syncthing-cert-pc.age".publicKeys = [ pc ];

  "kurnevsky-pinephone-pro.age".publicKeys = [ pinephone-pro ];
  "store-pinephone-pro.age".publicKeys = [ pinephone-pro ];

  "kurnevsky-pinenote.age".publicKeys = [ pinenote ];
  "store-pinenote.age".publicKeys = [ pinenote ];

  "kurnevsky-acer.age".publicKeys = [ acer ];
  "parents-acer.age".publicKeys = [ acer ];
  "store-acer.age".publicKeys = [ acer ];

  "kurnevsky-vps.age".publicKeys = [ vps ];
  "store-vps.age".publicKeys = [ vps ];
  "kropki.age".publicKeys = [ vps ];
  "stalwart.age".publicKeys = [ vps ];
  "syncthing-key-vps.age".publicKeys = [ vps ];
  "syncthing-cert-vps.age".publicKeys = [ vps ];

  "miniflux.age".publicKeys = [ vps ];
  "wakapi.age".publicKeys = [ vps ];
  "prometheus-wakapi.age".publicKeys = [ vps ];
  "tox.age".publicKeys = [ vps ];
  "continuwuity.age".publicKeys = [ vps ];
  "mautrix-whatsapp.age".publicKeys = [ vps ];
  "oauth2-proxy.age".publicKeys = [ vps ];
  "grafana.age".publicKeys = [ vps ];
  "scrutiny.age".publicKeys = [ vps ];
  "livekit.age".publicKeys = [ vps ];
  "coturn.age".publicKeys = [ vps ];
  "prosody-turn.age".publicKeys = [ vps ];
  "rustic.age".publicKeys = [ vps ];
  "storage.age".publicKeys = [ vps ];
  "anki.age".publicKeys = [ vps ];
  "pocket-id.age".publicKeys = [ vps ];

  "motion.age".publicKeys = desktop;
  "motion-xmpp.age".publicKeys = desktop;
  "motion-xmpp-recipients.age".publicKeys = desktop;
  "scrutiny-collector.age".publicKeys = desktop;

  "hans.age".publicKeys = all;
  "iodine.age".publicKeys = all;
  "shadowsocks.age".publicKeys = all;

  "wg-private-vps.age".publicKeys = [ vps ];

  "wg-preshared-dell.age".publicKeys = [
    vps
  ];

  "wg-private-evo.age".publicKeys = [ evo ];
  "wg-preshared-evo.age".publicKeys = [
    evo
    vps
  ];

  "wg-private-pc.age".publicKeys = [ pc ];
  "wg-preshared-pc.age".publicKeys = [
    pc
    vps
  ];

  "wg-private-acer.age".publicKeys = [ acer ];
  "wg-preshared-acer.age".publicKeys = [
    acer
    vps
  ];

  "github.age".publicKeys = all;
}
