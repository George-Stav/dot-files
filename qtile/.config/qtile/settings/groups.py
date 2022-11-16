# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile workspaces

from libqtile.config import Key, Group, Match
from libqtile.command import lazy
from .keys import mod, keys
from .helpers import get_session_type

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)

def games(wm_class):
    games = ["factorio", "PapersPlease", "osu!.exe", "dota2", "Minecraft Launcher"]
    for c in wm_class:
        if c.startswith("steam_app") or c in games or "minecraft" in c.lower():
            return True
    return False

groups = [
    Group(name="1", label="", layout="max"), #, matches=[Match(wm_class="firefox")]
    Group(name="2", label=""),
    Group(name="3", label="", matches=[Match(wm_class="Emacs")]),
    Group(name="4", label=""),
    Group(name="5", label="", matches=[Match(wm_class="virt-manager")]),
    Group(name="6", label="", matches=[Match(wm_class="qbittorrent")]),
    Group(name="7", label="", layout="max", matches=[Match(wm_class="Steam"), Match(wm_class="discord")]),
    Group(name="8", label="", layout="max"),
    Group(name="9", label="", matches=[Match(wm_class="Spotify")]),
    Group(name="0", label="", layout="max"), #, matches=[Match(wm_class="firefox")]
    Group(name="minus", label="", layout="max", matches=[Match(func=lambda c: games(c.get_wm_class()) if c.get_wm_class() else False)]), #, matches=[Match(wm_class="firefox")]
    # Group(name="minus", label="", layout="max"), #, matches=[Match(wm_class="firefox")]
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
        Key([mod, "control"], actual_key, lazy.window.togroup(group.name)), # , lazy.group[actual_key].toscreen()
    ])
