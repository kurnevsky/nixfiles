{ pkgs, ... }:

let
  homeDir = "/var/lib/motion";
  config = pkgs.writeText "motion.conf" ''
    videodevice /dev/video1
    width 1280
    height 720
    locate_motion_mode on
    locate_motion_style redbox
    threshold_tune on
    noise_tune on
    despeckle_filter EedDl
    minimum_motion_frames 2
    event_gap 20
    pre_capture 3
    post_capture 1
    picture_output first
    movie_codec hevc
    webcontrol_ipv6 on
    stream_port 8081
    stream_localhost off
    stream_auth_method 2
  '';
in {
  networking.firewall.allowedTCPPorts = [ 8081 ];

  users = {
    extraUsers.motion = {
      group = "motion";
      description = "Motion Service user";
      extraGroups = [ "video" ];
      home = homeDir;
      createHome = true;
      isSystemUser = true;
    };
    groups.motion = { };
  };

  systemd.services.motion = {
    description = "Motion daemon";
    after = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "on-failure";
      User = "motion";
      WorkingDirectory = homeDir;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ReadWritePaths = homeDir;
    };
    script = ''
      cat ${config} /secrets/motion > /tmp/motion.conf
      exec ${pkgs.motion}/bin/motion -n -c /tmp/motion.conf
    '';
  };
}
