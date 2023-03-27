{ pkgs, ... }:

let
  subnet = "192.168.43";
  controlPort = 37404;
  socksPort = 41718;
in {
  services.tor.settings = {
    CookieAuthentication = true;
    CookieAuthFileGroupReadable = true;
    ControlPort = [{
      addr = "${subnet}.1";
      port = controlPort;
    }];
    SOCKSPort = [{
      addr = "${subnet}.1";
      port = socksPort;
      IsolateDestAddr = true;
    }];
  };

  systemd.services.torbrowser-ns = with pkgs; {
    script = ''
      # Create a new network namespace named torbrowser
      ${iproute2}/bin/ip netns add torbrowser

      # Create two virtual ethernet interfaces
      ${iproute2}/bin/ip link add out-torbrowser type veth peer name in-torbrowser

      # Bind one interface to torbrowser network namespace
      ${iproute2}/bin/ip link set in-torbrowser netns torbrowser

      # Set interfaces ip and default routing
      ${iproute2}/bin/ip addr add ${subnet}.1/24 dev out-torbrowser
      ${iproute2}/bin/ip link set out-torbrowser up
      ${iproute2}/bin/ip netns exec torbrowser ${iproute2}/bin/ip addr add ${subnet}.2/24 dev in-torbrowser
      ${iproute2}/bin/ip netns exec torbrowser ${iproute2}/bin/ip link set in-torbrowser up
    '';
    serviceConfig = { Type = "oneshot"; };
    restartIfChanged = false;
    stopIfChanged = false;
  };

  networking.firewall.allowedTCPPorts = [ controlPort socksPort ];
}
