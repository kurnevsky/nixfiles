{
  pkgs,
  config,
  ...
}:

let
  systemd-mcp = pkgs.callPackage ./systemd-mcp-server.nix { };
in

{
  services.nginx = {
    # the mcp server doesn't support authentication on its own, so the token is
    # checked here - the secret contains a map entry like `"Bearer xxx" 1;` to
    # keep the token out of the nix store
    appendHttpConfig = ''
      map $http_authorization $systemd_mcp_authorized {
        default 0;
        include ${config.age.secrets.systemd-mcp.path or "/secrets/systemd-mcp"};
      }

      # a cors preflight carries no authorization header
      map $request_method $systemd_mcp_preflight {
        default 0;
        OPTIONS 1;
      }
    '';

    virtualHosts."kropki.org".locations."= /mcp" = {
      proxyPass = "http://localhost:34455";
      # it would override the Host header below
      recommendedProxySettings = false;
      extraConfig = ''
        set $systemd_mcp_deny "$systemd_mcp_preflight$systemd_mcp_authorized";
        if ($systemd_mcp_deny = 00) {
          return 401;
        }
        # it listens on loopback and rejects non-loopback Host headers
        # as a DNS rebinding protection
        proxy_set_header Host localhost;
        proxy_set_header "Connection" "";
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        # it's checked above and the server has no use for it
        proxy_set_header Authorization "";
        proxy_buffering off;
      '';
    };
  };

  systemd = {
    services = {
      systemd-mcp = {
        description = "Systemd MCP server";
        after = [ "gatekeeper.socket" ];
        wants = [ "gatekeeper.socket" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Restart = "on-failure";
          RestartSec = 5;
          DynamicUser = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          # the authorization is done by nginx, and only the read-only tools are
          # enabled - changing unit states would need polkit privileges anyway,
          # and get_file would expose arbitrary readable files
          ExecStart = "${systemd-mcp}/bin/systemd-mcp --http 127.0.0.1:34455 --noauth=ThisIsInsecure --allow-read";
        };
      };

      # it hands out file descriptors of the journal files to the clients that
      # are authorized by polkit, so that the mcp server doesn't need any
      # privileges to read the log itself
      gatekeeper = {
        description = "Gatekeeper service";
        requires = [ "gatekeeper.socket" ];
        serviceConfig = {
          Restart = "always";
          RestartSec = 5;
          User = "gatekeeper";
          Group = "gatekeeper";
          PrivateTmp = true;
          ProtectSystem = "strict";
          CapabilityBoundingSet = [ "CAP_DAC_READ_SEARCH" ];
          AmbientCapabilities = [ "CAP_DAC_READ_SEARCH" ];
          ExecStart = "${systemd-mcp}/bin/gatekeeper";
        };
      };
    };

    sockets.gatekeeper = {
      description = "Gatekeeper socket";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "/run/gatekeeper/gatekeeper.socket";
        # every client is authorized by polkit anyway
        SocketMode = "0666";
        RuntimeDirectory = "gatekeeper";
      };
    };
  };

  users = {
    users.gatekeeper = {
      group = "gatekeeper";
      isSystemUser = true;
    };
    groups.gatekeeper = { };
  };

  security.polkit = {
    enable = true;
    # the gatekeeper is the owner of the action, so it's allowed to check the
    # authorization of its clients
    extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (action.id == "com.suse.gatekeeper.readlog" && subject.user == "systemd-mcp") {
          return polkit.Result.YES;
        }
      });
    '';
  };

  # polkit looks for the actions in the system profile
  environment.systemPackages = [ systemd-mcp ];

  age.secrets.systemd-mcp = {
    file = ../../secrets/systemd-mcp.age;
    owner = config.services.nginx.user;
    group = config.services.nginx.group;
  };
}
