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
  '';
in {
  users.extraUsers.motion = {
    description = "Motion Service user";
    extraGroups = [ "video" ];
    home = homeDir;
    createHome = true;
    isSystemUser = true;
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
      ExecStart = "${pkgs.motion}/bin/motion -n -c ${config}";
    };
  };

  nixpkgs.overlays = [
    (self: super: {
      motion = super.motion.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [
          (pkgs.fetchpatch {
            name = "first-picture-output.patch";
            url =
              "https://patch-diff.githubusercontent.com/raw/Motion-Project/motion/pull/1136.patch";
            sha256 = "0g48j2xlzg3fifzqwy2llx1bb8wy685698bn0fjgz62m6fdpwr4j";
          })
        ];
      });
    })
  ];
}
