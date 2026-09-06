{ config, pkgs, ... }:

let
  homeDir = "/var/lib/motion";
  on-save = pkgs.writeShellScriptBin "on-save" ''
    echo "Motion detected!" | ${pkgs.lib.getExe pkgs.go-sendxmpp} \
      -f "$CREDENTIALS_DIRECTORY/xmpp" -h "$1" -r "$CREDENTIALS_DIRECTORY/xmpp-recipients"
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
      RuntimeDirectory = "motion";
      LoadCredential = [
        "config:${config.age.secrets.motion.path or "/secrets/motion"}"
        "xmpp:${config.age.secrets.motion-xmpp.path or "/secrets/motion-xmpp"}"
        "xmpp-recipients:${
          config.age.secrets.motion-xmpp-recipients.path or "/secrets/motion-xmpp-recipients"
        }"
      ];
    };
    script = ''
      cat ${motion-config} "$CREDENTIALS_DIRECTORY/config" > "$RUNTIME_DIRECTORY/motion.conf"
      exec ${pkgs.motion}/bin/motion -n -c "$RUNTIME_DIRECTORY/motion.conf"
    '';
  };

  age.secrets = {
    motion.file = ../secrets/motion.age;
    motion-xmpp.file = ../secrets/motion-xmpp.age;
    motion-xmpp-recipients.file = ../secrets/motion-xmpp-recipients.age;
  };
}
