# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile workspaces

from libqtile.config import Key, Group, Match
from libqtile.command import lazy
from .keys import mod, keys
from .helpers import get_session_type

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)


groups = [
    Group(name="1", label=" "),
    Group(name="2", label=" "),
    Group(name="3", label=" "),
    Group(name="4", label=" "),
    Group(name="5", label=" "),
    Group(name="6", label=" "),
    Group(name="7", label=" "),
    Group(name="8", label=" "),
    Group(name="9", label=" "),
 ]

groups_work = [
    Group(name="1", label="   "),
    Group(name="2", label="   "),
    Group(name="3", label="   "),
    Group(name="4", label="   "),
    Group(name="5", label="   "),
    Group(name="6", label="   "),
    Group(name="7", label="   "),
    Group(name="8", label="   "),
    Group(name="9", label="   "),
]

for group in groups:
    actual_key = group.name
    keys.extend([
        # Switch to workspace N
        Key([mod], actual_key, lazy.group[group.name].toscreen()),
        # Send window to workspace N
        Key([mod, "control"], actual_key, lazy.window.togroup(group.name), lazy.group[actual_key].toscreen()),
    ])
