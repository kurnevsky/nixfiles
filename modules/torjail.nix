{ pkgs, ... }:

let
  subnet = "192.168.42";
  transPort = 9041;
  dnsPort = 9054;
in
{
  services.tor.settings = {
    TransPort = [
      {
        addr = "${subnet}.1";
        port = transPort;
      }
    ];
    DNSPort = [
      {
        addr = "${subnet}.1";
        port = dnsPort;
      }
    ];
  };

  networking.nftables.ruleset = ''
    table ip torjail_nat {
      chain prerouting {
        type nat hook prerouting priority dstnat; policy accept;

        # Forward DNS traffic to Tor DNSPort
        iifname "out-torjail" ip protocol udp ip daddr ${subnet}.1 udp dport 53 dnat to ${subnet}.1:${toString dnsPort}

        # Forward TCP traffic (SYN packets only) to Tor TransPort
        iifname "out-torjail" tcp flags & (fin|syn|rst|ack) == syn dnat to ${subnet}.1:${toString transPort}
      }

      chain input {
        type filter hook input priority filter; policy accept;

        # Accept traffic redirected to the Tor DNS Port
        iifname "out-torjail" ip daddr ${subnet}.1 udp dport ${toString dnsPort} accept

        # Accept traffic redirected to the Tor TransPort
        iifname "out-torjail" ip daddr ${subnet}.1 tcp dport ${toString transPort} accept

        # Include UDP TransPort if required (based on your original script)
        iifname "out-torjail" ip daddr ${subnet}.1 udp dport ${toString transPort} accept

        # Drop everything else originating from the jail interface
        iifname "out-torjail" drop
      }

      chain output {
        type filter hook output priority filter; policy accept;

        # Allow established connections back to the jail
        oifname "out-torjail" ct state established,related accept
      }
    }
  '';

  systemd.services = {
    torjail-ns = with pkgs; {
      script = ''
        # Create a new network namespace named torjail
        ${iproute2}/bin/ip netns add torjail

        # Create two virtual ethernet interfaces
        ${iproute2}/bin/ip link add out-torjail type veth peer name in-torjail

        # Bind one interface to torjail network namespace
        ${iproute2}/bin/ip link set in-torjail netns torjail

        # Set interfaces ip and default routing
        ${iproute2}/bin/ip addr add ${subnet}.1/24 dev out-torjail
        ${iproute2}/bin/ip link set out-torjail up
        ${iproute2}/bin/ip netns exec torjail ${iproute2}/bin/ip addr add ${subnet}.2/24 dev in-torjail
        ${iproute2}/bin/ip netns exec torjail ${iproute2}/bin/ip link set in-torjail up
        ${iproute2}/bin/ip netns exec torjail ${iproute2}/bin/ip route add default via ${subnet}.1
      '';
      serviceConfig = {
        Type = "oneshot";
      };
      restartIfChanged = false;
      stopIfChanged = false;
    };
    tor.requires = [ "torjail-ns.service" ];
  };

  networking.firewall = {
    allowedTCPPorts = [ transPort ];
    allowedUDPPorts = [ dnsPort ];
  };

  environment.etc."resolv-torjail.conf".text = "nameserver ${subnet}.1";
}
