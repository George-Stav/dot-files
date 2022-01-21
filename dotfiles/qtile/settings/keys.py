#https://jupyter.packtpub.services/lab/tree/find_table_occurances.sh Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile keybindings

from libqtile.config import Key
from libqtile.command import lazy
from libqtile.utils import send_notification
from .helpers import get_monitor_position


mod = "mod1"
scripts_dir = "/home/george/dev/scripts/scripts/"

# @lazy.function
# def cycle_groups(qtile, key="prev"):
#     if key == "next":
#         group = qtile.current_screen.group.get_next_group()
#     else:
#         group = qtile.current_screen.group.get_previous_group()
#     qtile.current_screen.set_group(group)

# def latest_group(qtile):
#     qtile.current_screen.set_group(qtile.current_screen.previous_group)

@lazy.function
def testing_mon(qtile, command="prev"):
    # prev = left
    # next = right
    pos = get_monitor_position(f'{qtile.current_screen.cmd_info()["index"]}')
    # qtile.cmd_spawn(f'rofi -e "{pos} | {command}"')

    if pos == "left" and command == "next":
        # do screen.next_screen()
        qtile.cmd_next_screen()
        # qtile.cmd_spawn(f'rofi -e "go to right monitor"')
    elif pos == "right" and command == "prev":
        # do screen.prev_screen()
        qtile.cmd_prev_screen()
        # qtile.cmd_spawn(f'rofi -e "go to left monitor"')

@lazy.function
def mic_toggle(qtile):
    qtile.cmd_spawn('mic-toggle') # toggle mic using amixer and push notification
    qtile.widgets_map['micstate'].toggle() # change widget icon


keys = [Key(key[0], key[1], *key[2:]) for key in [
    # ------------ Testing ------------

    # ([mod, "control"], "Return", testing),
    # ([mod, "shift"], "Return", testing_mon),
    # ([mod, "control"], "Return", mic_toggle),

    # ------------ Group Configs ------------

    # Cycle through groups
    ([mod, "control"], "comma", lazy.screen.prev_group()),
    ([mod, "control"], "period", lazy.screen.next_group()),

    # Toggle between the latest 2 groups
    ([mod], "grave", lazy.screen.toggle_group()),

    # ------------ Window Configs ------------

    # Switch focus of monitors
    ([mod], "comma", lazy.next_screen()),
    ([mod], "period", lazy.prev_screen()),

    ([mod, "shift"], "m", lazy.window.toggle_minimize()),
    ([mod, "shift"], "f", lazy.window.toggle_fullscreen()),

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
    ([mod, "control"], "f", lazy.window.toggle_floating()),

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

    # Restart Qtile
    ([mod, "control"], "r", lazy.restart()),
    ([mod, "control"], "q", lazy.shutdown()),

    # ------------ App Configs ------------

    # Rofi
    # -m -4: rofi prompt appears on the screen with the focused window
    ([mod], "m", lazy.spawn("rofi -m -4 -modi drun -show drun")),
    ([mod], "r", lazy.spawn("rofi -m -4 -show run")),
    ([mod], "w", lazy.spawn("rofi -m -4 -show window")),

    # Browser
    ([mod], "b", lazy.spawn("firefox")),

    # Emacs
    ([mod], "e", lazy.spawn("emacsclient -c")),
    ([mod, "shift"], "e", lazy.spawn("systemctl --user restart emacs")),
    ([mod], "d", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'")),

    # File Explorer
    # ([mod], "e", lazy.spawn("pcmanfm")),

    # Terminal
    ([mod], "Return", lazy.spawn("alacritty")),

    # Redshift
    # ([mod], "r", lazy.spawn("redshift -O 2400")),
    # ([mod, "shift"], "r", lazy.spawn("redshift -x")),

    # Screenshot
    # ([mod], "s", lazy.spawn("scrot")),
    # ([mod, "shift"], "s", lazy.spawn("scrot -s")),
    ([mod, "shift"], "s", lazy.spawn("flameshot gui")),

    # ------------ Utility Configs ------------

    ([mod], "space", lazy.spawn('cycle-kb-layout')),
    (["shift"], "XF86AudioMute", mic_toggle),

    # ------------ Hardware Configs ------------

    # Volume
    ([], "XF86AudioLowerVolume", lazy.spawn(
        "change-volume -2%"
    )),
    ([], "XF86AudioRaiseVolume", lazy.spawn(
        "change-volume +2%"
    )),
    ([], "XF86AudioMute", lazy.spawn(
        "change-volume 0"
    )),
    ([], "XF86AudioPlay", lazy.spawn(
        "playerctl play-pause"
    )),
    ([], "XF86AudioPrev", lazy.spawn(
        "playerctl previous"
    )),
    ([], "XF86AudioNext", lazy.spawn(
        "playerctl next"
    )),

    # Brightness
    ([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +5%")),
    ([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 5%-")),
]]
