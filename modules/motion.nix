{ config, pkgs, ... }:

let
  homeDir = "/var/lib/motion";
  on-save = pkgs.writeShellScriptBin "on-save" ''
    echo "Motion detected!" | ${pkgs.lib.getExe pkgs.go-sendxmpp} -f ${
      config.age.secrets.motion-xmpp.path or "/secrets/motion-xmpp"
    } -h "$1" -r ${config.age.secrets.motion-xmpp-recipients.path or "/secrets/motion-xmpp-recipients"}
  '';
  motion-config = pkgs.writeText "motion.conf" ''
    video_device /dev/video1
    width 1920
    height 1080
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
    on_picture_save ${pkgs.lib.getExe on-save} %f
  '';
in
{
  networking.firewall.allowedTCPPorts = [ 8081 ];

  users = {
    extraUsers.motion = {
      group = "motion";
      description = "Motion Service user";
      extraGroups = [ "video" ];
      home = homeDir;
      homeMode = "755";
      createHome = true;
      isSystemUser = true;
    };
    groups = {
      motion = { };
      secrets-motion = { };
    };
  };

  systemd.services.motion = {
    description = "Motion daemon";
    after = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "on-failure";
      User = "motion";
      SupplementaryGroups = "secrets-motion";
      WorkingDirectory = homeDir;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ReadWritePaths = homeDir;
    };
    script = ''
      cat ${motion-config} ${config.age.secrets.motion.path or "/secrets/motion"} > /tmp/motion.conf
      exec ${pkgs.motion}/bin/motion -n -c /tmp/motion.conf
    '';
  };

  age.secrets = {
    motion = {
      file = ../secrets/motion.age;
      mode = "440";
      group = "secrets-motion";
    };
    motion-xmpp = {
      file = ../secrets/motion-xmpp.age;
      mode = "440";
      group = "secrets-motion";
    };
    motion-xmpp-recipients = {
      file = ../secrets/motion-xmpp-recipients.age;
      mode = "440";
      group = "secrets-motion";
    };
  };
}
