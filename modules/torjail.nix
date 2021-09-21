{ pkgs, ... }:

let
  subnet = "192.168.42";
  transPort = 9041;
  dnsPort = 9054;
in {
  services.tor.settings = {
    TransPort = [{
      addr = "${subnet}.1";
      port = transPort;
    }];
    DNSPort = [{
      addr = "${subnet}.1";
      port = dnsPort;
    }];
  };

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

        # Forward all dns traffic to tor DNSPort
        ${iptables}/bin/iptables -t nat -A PREROUTING -i out-torjail -p udp -d ${subnet}.1 --dport 53 -j DNAT --to-destination ${subnet}.1:${
          toString dnsPort
        }

        # Forward all traffic to tor TransPort
        ${iptables}/bin/iptables -t nat -A PREROUTING -i out-torjail -p tcp --syn -j DNAT --to-destination ${subnet}.1:${
          toString transPort
        }

        # Accept established connection
        ${iptables}/bin/iptables -A OUTPUT -m state -o out-torjail --state ESTABLISHED,RELATED -j ACCEPT

        # Accept only forwarded traffic
        ${iptables}/bin/iptables -A INPUT -i out-torjail -p udp --destination ${subnet}.1 --dport ${
          toString dnsPort
        } -j ACCEPT
        ${iptables}/bin/iptables -A INPUT -i out-torjail -p tcp --destination ${subnet}.1 --dport ${
          toString transPort
        } -j ACCEPT
        ${iptables}/bin/iptables -A INPUT -i out-torjail -p udp --destination ${subnet}.1 --dport ${
          toString transPort
        } -j ACCEPT
        ${iptables}/bin/iptables -A INPUT -i out-torjail -j DROP
      '';
      serviceConfig = { Type = "oneshot"; };
    };
    tor.requires = [ "torjail-ns.service" ];
  };

  networking.firewall = {
    allowedTCPPorts = [ transPort ];
    allowedUDPPorts = [ dnsPort ];
  };

  environment.etc."resolv-torjail.conf".text = "nameserver ${subnet}.1";
}
