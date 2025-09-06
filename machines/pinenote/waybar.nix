{
  layer = "top";

  position = "top";

  # If height property would be not present, it'd be calculated dynamically
  height = 40;

  modules-left = [
    "custom/smenu"
    "custom/okb"
    "custom/mvws_prev"
    "custom/gows_prev"
    "custom/gows_next"
    "custom/mvws_next"
    "custom/windown"
    "custom/winright"
    "custom/splitv"
    "custom/splith"
  ];

  modules-center = [
    "sway/mode"
  ];

  modules-right = [
    "custom/ebc_dump_buffers"
    "custom/ebc_cycle_driver_mode"
    "custom/ebc_refresh"
    "custom/blc_down"
    "backlight/slider#cool"
    "custom/blc_up"
    "custom/blw_down"
    "backlight/slider#warm"
    "custom/blw_up"
    "idle_inhibitor"
    "battery"
    "tray"
    # "clock#time"
    "custom/kill"
  ];

  "backlight/slider#cool" = {
    device = "backlight_cool";
  };

  "backlight/slider#warm" = {
    device = "backlight_warm";
  };

  battery = {
    bat = "rk817-battery";
    name = "battery";
    interval = 10;
    states = {
      warning = 30;
      critical = 15;
    };
    # Connected to AC
    format = " {icon} {capacity}%";
    # Not connected to AC
    format-discharging = "{capacity}% {icon}";
    format-icons = [
      ""
      ""
      ""
      ""
      ""
    ];
    min-length = 5;
    tooltip = true;
  };

  "clock#time" = {
    interval = 1;
    format = "{:%H:%M}";
    tooltip = false;
  };

  "clock#date" = {
    interval = 10;
    format = "  {:%e %b %Y}";
    tooltip-format = "{:%e %B %Y}";
  };

  "cpu" = {
    interval = 5;
    format = "  {usage}%";
    states = {
      warning = 70;
      critical = 90;
    };
  };

  "memory" = {
    interval = 5;
    format = "  {}%";
    states = {
      warning = 70;
      critical = 90;
    };
  };

  "sway/mode" = {
    format = "<span style=\"italic\">  {}</span>";
    tooltip = false;
  };

  "sway/window" = {
    format = "{}";
    max-length = 120;
  };

  temperature = {
    critical-threshold = 80;
    interval = 5;
    format = "{icon}  {temperatureC}°C";
    format-icons = [
      ""
      ""
      ""
      ""
      ""
    ];
    tooltip = true;
    hwmon-path = "/sys/class/hwmon/hwmon3/temp1_input";
  };

  "custom/kill" = {
    format = "";
    interval = "once";
    on-click = "swaymsg kill";
    min-length = 5;
    tooltip = false;
  };

  "custom/winleft" = {
    format = "";
    interval = "once";
    on-click = "swaymsg move left";
    min-length = 5;
    tooltip = false;
  };

  "custom/winright" = {
    format = "";
    interval = "once";
    on-click = "swaymsg move right";
    min-length = 5;
    tooltip = false;
  };

  "custom/winup" = {
    format = ";";
    interval = "once";
    on-click = "swaymsg move up";
    min-length = 5;
    tooltip = false;
  };

  "custom/windown" = {
    format = "";
    interval = "once";
    on-click = "swaymsg move down";
    min-length = 5;
    tooltip = false;
  };

  "custom/splitv" = {
    format = "/;";
    interval = "once";
    on-click = "swaymsg splitv";
    min-length = 5;
    tooltip = false;
  };

  "custom/splith" = {
    format = "/;";
    interval = "once";
    on-click = "swaymsg splith";
    min-length = 5;
    tooltip = false;
  };

  "custom/mvws_prev" = {
    format = "";
    interval = "once";
    on-click = "sway_workspace move prev";
    min-length = 5;
    tooltip = false;
  };

  "custom/gows_prev" = {
    format = "<";
    interval = "once";
    on-click = "sway_workspace goto prev";
    min-length = 5;
    tooltip = false;
  };

  "custom/gows_next" = {
    format = ">";
    interval = "once";
    on-click = "sway_workspace goto next";
    min-length = 5;
    tooltip = false;
  };

  "custom/mvws_next" = {
    format = "";
    interval = "once";
    on-click = "sway_workspace move next";
    min-length = 5;
    tooltip = false;
  };

  "custom/okb" = {
    format = "";
    interval = "once";
    on-click = "toggle_onscreen_keyboard.py";
    min-length = 5;
    tooltip = false;
  };

  "custom/smenu" = {
    format = "";
    interval = "once";
    on-click = "toggle_menu.sh";
    min-length = 5;
    tooltip = false;
  };

  "custom/ws1" = {
    format = "1";
    interval = "once";
    on-click = "swaymsg workspace number 1";
    min-length = 5;
    tooltip = false;
  };

  "custom/ws2" = {
    format = "2";
    interval = "once";
    on-click = "swaymsg workspace number 2";
    min-length = 5;
    tooltip = false;
  };

  "custom/ws3" = {
    format = "3";
    interval = "once";
    on-click = "swaymsg workspace number 3";
    min-length = 5;
    tooltip = false;
  };

  "custom/ws4" = {
    format = "4";
    interval = "once";
    on-click = "swaymsg workspace number 4";
    min-length = 5;
    tooltip = false;
  };

  "custom/ws5" = {
    format = "5";
    interval = "once";
    on-click = "swaymsg workspace number 5";
    min-length = 5;
    tooltip = false;
  };

  "custom/blc_down" = {
    format = "";
    interval = "once";
    on-click = "brightnessctl --device=backlight_cool set 10%-";
    min-length = 5;
    tooltip = false;
  };

  "custom/blc_up" = {
    format = "";
    interval = "once";
    on-click = "brightnessctl --device=backlight_cool set 10%+";
    min-length = 5;
    tooltip = false;
  };

  "custom/blw_down" = {
    format = "";
    interval = "once";
    on-click = "brightnessctl --device=backlight_warm set 10%-";
    min-length = 5;
    tooltip = false;
  };

  "custom/blw_up" = {
    format = "";
    interval = "once";
    on-click = "brightnessctl --device=backlight_warm set 10%+";
    min-length = 5;
    tooltip = false;
  };

  "idle_inhibitor" = {
    format = "{icon}";
    format-icons = {
      activated = "";
      deactivated = "";
    };
    min-length = 5;
  };

  "custom/ebc_dump_buffers" = {
    format = "🐃";
    interval = "once";
    on-click = "rockchip_ebc_dump_buffers.py";
    min-length = 5;
    tooltip = false;
  };

  "custom/ebc_cycle_driver_mode" = {
    format = "🚲";
    interval = "once";
    on-click = "python -c 'import rockchip_ebc_custom_ioctl as reci; reci.cycle_driver_mode()'";
    min-length = 5;
    tooltip = false;
  };

  "custom/ebc_refresh" = {
    format = "";
    interval = "once";
    on-click = "dbus-send --type=method_call --dest=org.pinenote.ebc_custom / org.pinenote.ebc_custom.GlobalRefresh";
    min-length = 5;
    tooltip = false;
  };

  "custom/rotate_0" = {
    format = "R0";
    interval = "once";
    on-click = "sway_rotate.sh rotnormal";
    min-length = 5;
    tooltip = false;
  };

  "custom/rotate_90" = {
    format = "R90";
    interval = "once";
    on-click = "sway_rotate.sh rotright";
    min-length = 5;
    tooltip = false;
  };

  "custom/rotate_180" = {
    format = "R180";
    interval = "once";
    on-click = "sway_rotate.sh rotinvert";
    min-length = 5;
    tooltip = false;
  };

  "custom/rotate_270" = {
    format = "R270";
    interval = "once";
    on-click = "sway_rotate.sh rotleft";
    min-length = 5;
    tooltip = false;
  };

  "custom/key_pageup" = {
    format = "";
    interval = "once";
    on-click = "swaymsg resize grow width 10px; swaymsg resize grow height 10px";
    min-length = 5;
    tooltip = false;
  };

  "custom/key_pagedown" = {
    format = "";
    interval = "once";
    on-click = "swaymsg resize shrink width 10px; swaymsg resize shrink height 10px";
    min-length = 5;
    tooltip = false;
  };

  "custom/battery_watts" = {
    exec = "battery_watts.sh";
    format = " {}W";
    interval = 10;
    min-length = 5;
    tooltip = false;
  };

  tray = {
    icon-size = 21;
    spacing = 10;
  };
}
