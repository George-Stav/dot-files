#https://jupyter.packtpub.services/lab/tree/find_table_occurances.sh Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile keybindings

from libqtile.config import Key, KeyChord
from libqtile.config import Match
from libqtile.command import lazy
from libqtile.utils import send_notification
from .helpers import get_monitor_position
from .path import scripts_path


mod = "mod1"
windows = "mod4"

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
def mic_toggle(qtile):
    qtile.widgets_map['togglestate'].toggle() # change widget icon
    qtile.cmd_spawn(f'{scripts_path}/mic-toggle') # toggle mic using amixer and push notification

@lazy.function
def play_pause(qtile, spotify=False):
    if not spotify:
        qtile.cmd_spawn('playerctl play-pause')
        qtile.cmd_spawn(f'{scripts_path}/media-notif')
    else:
        qtile.cmd_spawn('playerctl -p spotify play-pause')
        qtile.cmd_spawn(f'{scripts_path}/media-notif spotify')

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
def test_widgets(qtile):
    widgets = [w for w in qtile.cmd_list_widgets()]
    qtile.cmd_spawn(f'rofi -e {len(qtile.cmd_list_widgets())}')


keys = [
    # ------------ Testing ------------

    # ([mod, "control"], "Return", testing),
    # ([mod, "shift"], "Return", testing_mon),
    Key([mod, "control"], "Return", test_widgets),

    # ------------ Group Configs ------------

    # Cycle through groups
    Key([mod, "control"], "comma", lazy.screen.prev_group()),
    Key([mod, "control"], "period", lazy.screen.next_group()),

    # Toggle between the latest 2 groups
    Key([mod], "grave", lazy.screen.toggle_group()),

    # ------------ Window Configs ------------

    # Switch focus of monitors
    Key([mod], "comma", lazy.next_screen()),
    Key([mod], "period", lazy.prev_screen()),

    Key([mod, "shift"], "m", lazy.window.toggle_minimize()),
    Key([mod, "shift"], "f", lazy.window.toggle_fullscreen()),

    # Switch between windows in current stack pane
    Key([mod], "j", lazy.layout.down()),
    Key([mod], "k", lazy.layout.up()),
    Key([mod], "h", lazy.layout.left()),
    Key([mod], "l", lazy.layout.right()),

    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left()),
    Key([mod, "control"], "l", lazy.layout.grow_right()),
    Key([mod, "control"], "j", lazy.layout.grow_down()),
    Key([mod, "control"], "k", lazy.layout.grow_up()),
    Key([mod], "n", lazy.layout.normalize()),

    # Toggle floating
    Key([mod, "control"], "f", lazy.window.toggle_floating()),

    # Move windows up or down in current stack
    Key([mod, "shift"], "j", lazy.layout.shuffle_down()),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up()),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right()),
    Key([mod, "shift"], "h", lazy.layout.shuffle_left()),

    # Toggle between different layouts as defined below
    Key([mod], "Tab", lazy.next_layout()),
    Key([mod, "shift"], "Tab", lazy.prev_layout()),

    # Kill window
    Key([mod], "q", lazy.window.kill()),

    # Restart Qtile
    Key([mod, "control"], "r", lazy.restart()),
    Key([mod, "control"], "q", lazy.shutdown()),

    # ------------ App Configs ------------

    # Rofi
    # -m -4: rofi prompt appears on the screen with the focused window
    Key([mod], "m", lazy.spawn("rofi -m -4 -modi drun -show drun")),
    Key([mod], "r", lazy.spawn("rofi -m -4 -show run")),
    Key([mod], "space", lazy.spawn("rofi -m -4 -show window")),

    # Browser
    Key([mod], "b", lazy.spawn("firefox")),

    # Emacs
    Key([mod], "e", lazy.spawn("emacsclient -c")),
    Key([mod, "shift"], "e", lazy.spawn("systemctl --user restart emacs")),
    # Key([mod], "d", lazy.spawn("emacsclient -c -a 'emacs' --eval '(dired nil)'")),
    Key([mod], "d", lazy.spawn(f"alacritty -e {scripts_path}/lfrun")),

    # File Explorer
    # Key([mod], "e", lazy.spawn("pcmanfm")),

    # Terminal
    Key([mod], "Return", lazy.spawn("alacritty")),

    # Redshift
    # Key([mod], "r", lazy.spawn("redshift -O 2400")),
    # Key([mod, "shift"], "r", lazy.spawn("redshift -x")),

    # Screenshot
    # Key([mod], "s", lazy.spawn("scrot")),
    # Key([mod, "shift"], "s", lazy.spawn("scrot -s")),
    Key([mod, "shift"], "s", lazy.spawn("flameshot gui")),
    # Key([], "print", lazy.spawn("flameshot gui")),

    KeyChord([windows], "x", [
        Key([], "t", lazy.spawn("xpad -t")),
        Key([], "n", lazy.spawn("xpad -n"))
    ]),

    # ------------ Utility Configs ------------

    Key([mod, "control"], "space", lazy.spawn(f'{scripts_path}/cycle-kb-layout')),
    Key(["shift"], "XF86AudioMute", mic_toggle),
    Key([], "XF86AudioMicMute", mic_toggle),
    Key([], "XF86Favorites", mic_toggle),

    # ------------ Hardware Configs ------------

    # Volume
    Key([], "XF86AudioLowerVolume", lazy.spawn(
        f"{scripts_path}/change-volume -2%"
    )),
    Key(["shift"], "XF86AudioLowerVolume", lazy.spawn(
        f"{scripts_path}/change-volume -10%"
    )),
    Key([], "XF86AudioRaiseVolume", lazy.spawn(
        f"{scripts_path}/change-volume +2%"
    )),
    Key(["shift"], "XF86AudioRaiseVolume", lazy.spawn(
        f"{scripts_path}/change-volume +10%"
    )),
    Key([], "XF86AudioMute", lazy.spawn(
        f"{scripts_path}/change-volume 0"
    )),
    Key([], "XF86AudioPlay", play_pause()),
    Key(["shift"], "XF86AudioPlay", play_pause(spotify=True)),
    Key([], "XF86AudioPrev", lazy.spawn(
        "playerctl previous"
    )),
    Key([], "XF86AudioNext", lazy.spawn(
        "playerctl next"
    )),

    # Brightness
    Key([], "XF86MonBrightnessUp", lazy.spawn("brightnessctl set +5%")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("brightnessctl set 5%-")),
]
