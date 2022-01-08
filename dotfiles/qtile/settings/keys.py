# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile keybindings

from libqtile.config import Key
from libqtile.command import lazy


mod = "mod1"

keys = [Key(key[0], key[1], *key[2:]) for key in [
    # ------------ Window Configs ------------

    # Switch between windows in current stack pane
    ([mod], "j", lazy.layout.down()),
    ([mod], "k", lazy.layout.up()),
    ([mod], "h", lazy.layout.left()),
    ([mod], "l", lazy.layout.right()),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    ([mod, "control"], "h", lazy.layout.grow_left()),
    ([mod, "control"], "l", lazy.layout.grow_right()),
    ([mod, "control"], "j", lazy.layout.grow_down()),
    ([mod, "control"], "k", lazy.layout.grow_up()),
    ([mod], "n", lazy.layout.normalize()),

    # Toggle floating
    ([mod, "shift"], "f", lazy.window.toggle_floating()),

    # Move windows up or down in current stack
    ([mod, "shift"], "j", lazy.layout.shuffle_down()),
    ([mod, "shift"], "k", lazy.layout.shuffle_up()),
    ([mod, "shift"], "l", lazy.layout.shuffle_right()),
    ([mod, "shift"], "h", lazy.layout.shuffle_left()),

    # Toggle between different layouts as defined below
    ([mod], "Tab", lazy.next_layout()),
    ([mod, "shift"], "Tab", lazy.prev_layout()),

    # Kill window
    ([mod], "q", lazy.window.kill()),

    # Switch focus of monitors
    ([mod], "period", lazy.next_screen()),
    ([mod], "comma", lazy.prev_screen()),

    # Restart Qtile
    ([mod, "control"], "r", lazy.restart()),

    ([mod, "control"], "q", lazy.shutdown()),
    ([mod], "e", lazy.spawncmd()),

    # ------------ App Configs ------------

    # Rofi
    ([mod, "shift"], "m", lazy.spawn("rofi -show")),
    ([mod], "m", lazy.spawn("rofi -show drun")),
    ([mod], "r", lazy.spawn("rofi -show run")),
    ([mod], "w", lazy.spawn("rofi -show window")),

    # Browser
    ([mod], "b", lazy.spawn("firefox")),

    # Emacs
    ([mod], "a", lazy.spawn("emacsclient -c")),
    ([mod, "shift"], "a", lazy.spawn("systemctl --user restart emacs")),

    # File Explorer
    ([mod], "e", lazy.spawn("pcmanfm")),

    # Terminal
    ([mod], "Return", lazy.spawn("alacritty")),

    # Redshift
    # ([mod], "r", lazy.spawn("redshift -O 2400")),
    # ([mod, "shift"], "r", lazy.spawn("redshift -x")),

    # Screenshot
    ([mod], "s", lazy.spawn("scrot")),
    ([mod, "shift"], "s", lazy.spawn("scrot -s")),

    # ------------ Hardware Configs ------------

    # Volume
    ([], "XF86AudioLowerVolume", lazy.spawn(
        "pactl set-sink-volume @DEFAULT_SINK@ -2%"
    )),
    ([], "XF86AudioRaiseVolume", lazy.spawn(
        "pactl set-sink-volume @DEFAULT_SINK@ +2%"
    )),
    ([], "XF86AudioMute", lazy.spawn(
        "pactl set-sink-mute @DEFAULT_SINK@ toggle"
    )),

    # Brightness
    ([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +5%")),
    ([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 5%-")),
]]
