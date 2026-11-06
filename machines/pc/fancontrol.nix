{ pkgs, lib, ... }:

# Software fan control.
#
# Airflow:
#
#             TOP: fan1, fan5 (exhaust, up)
#   FRONT  fan6 (intake) → CPU cooler fan2 → fan3 (exhaust, rear)
#   FRONT  fan4 (intake) → GPU 1, GPU 2
#
# Header mapping (pwmN drives fanN):
#   pwm1,5 - top exhaust, main exit for GPU heat, follow
#            max(CPU package, GPU junctions - 10 °C)
#   pwm2   - CPU cooler, left to the BIOS curve (not listed here)
#   pwm3   - rear exhaust, CPU level, follows
#            max(CPU package, GPU junctions - 20 °C)
#   pwm4   - front lower intake, blows directly into both GPUs, follows the
#            hotter GPU junction
#   pwm6   - front upper intake, CPU level, same sensor as pwm3
#   pwm7   - nothing with a tachometer, left to the BIOS
# Headers not listed below stay under BIOS (SmartFan) control.

let
  # Prints the hottest of the given sensors in millidegrees. Each sensor is
  # "<hwmon name>:<tempN>:<offset in millidegrees subtracted from the reading>".
  # hwmon directories are resolved by name because their numbering is not
  # stable. Unreadable sensors (e.g. a runtime-suspended GPU) are skipped; if
  # none can be read, reports 100 °C so the fans fail safe to full speed.
  maxTemp =
    name: sensors:
    pkgs.writeShellScript "fancontrol-${name}" ''
      max=-1000000
      found=0
      for h in /sys/class/hwmon/hwmon*; do
        n=$(<"$h/name")
        for s in ${lib.concatStringsSep " " sensors}; do
          IFS=: read -r sname temp offset <<< "$s"
          [[ $sname == "$n" ]] || continue
          read -r t < "$h/''${temp}_input" 2>/dev/null || continue
          found=1
          t=$((t - offset))
          ((t > max)) && max=$t
        done
      done
      if ((found)); then echo "$max"; else echo 100000; fi
    '';

  gpu = maxTemp "gpu" [ "amdgpu:temp2:0" ];
  case = maxTemp "case" [
    "coretemp:temp1:0"
    "amdgpu:temp2:20000"
  ];
  top = maxTemp "top" [
    "coretemp:temp1:0"
    "amdgpu:temp2:10000"
  ];

  # Stable path to the NCT6798 hwmon directory, created in preStart.
  # nct6775.656 is the chip's I/O port (0x290) and doesn't change.
  dir = "/run/nct6798";

  caseFan = temp: minpwm: {
    inherit temp;
    mintemp = 45;
    maxtemp = 80;
    inherit minpwm;
    minstart = minpwm + 40;
    average = 10;
  };

  fans = {
    # Measured: fan1/fan5 never stop (~350 RPM minimum, same at duty 30 and 60);
    # fan3/fan6 stall between duty 30 and 60, so they get a margin above 60.
    pwm1 = caseFan top 60;
    pwm3 = caseFan case 70;
    pwm4 = {
      temp = gpu;
      mintemp = 60;
      maxtemp = 95;
      minpwm = 100;
      minstart = 140;
      average = 5;
    };
    pwm5 = caseFan top 60;
    pwm6 = caseFan case 70;
  };

  setting =
    key: f:
    "${key}="
    + lib.concatStringsSep " " (lib.mapAttrsToList (pwm: fan: "${dir}/${pwm}=${f pwm fan}") fans);
in
{
  hardware.fancontrol = {
    enable = true;
    config = lib.concatStringsSep "\n" [
      "INTERVAL=2"
      (setting "FCTEMPS" (_: fan: "!${fan.temp}"))
      (setting "FCFANS" (pwm: _: "${dir}/fan${lib.removePrefix "pwm" pwm}_input"))
      (setting "MINTEMP" (_: fan: toString fan.mintemp))
      (setting "MAXTEMP" (_: fan: toString fan.maxtemp))
      (setting "MINSTART" (_: fan: toString fan.minstart))
      (setting "MINSTOP" (_: fan: toString fan.minpwm))
      (setting "MINPWM" (_: fan: toString fan.minpwm))
      (setting "MAXPWM" (_: _: "255"))
      (setting "AVERAGE" (_: fan: toString fan.average))
      ""
    ];
  };

  systemd.services.fancontrol.preStart = ''
    ln -sfn /sys/devices/platform/nct6775.656/hwmon/hwmon* ${dir}
  '';
}
