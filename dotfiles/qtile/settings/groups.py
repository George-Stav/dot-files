# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile workspaces

from libqtile.config import Key, Group, Match
from libqtile.command import lazy
from .keys import mod, keys
from .helpers import get_session_type

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)

# labels_casual = [
#     "   ", "   ", "   ", "   ", "   ", "   ", "   ", " 嗢  ", "   "
# ]

# labels_work = [
#     "   ", "   ", "   ", "   ", "   ", "   ", "   ", "   ", "   "
# ]

# groups = []
# if get_session_type == 2: # Work session
#     groups = [
#         Group(name="1", label="   "),
#         Group(name="2", label="   "),
#         Group(name="3", label="   ",
#               spawn=["emacsclient -c"]),
#         Group(name="4", label="   "),
#         Group(name="5", label="   ",
#               spawn=["dbeaver"]),
#         Group(name="6", label="   "),
#         Group(name="7", label="   ",
#               spawn=["teams"]),
#         Group(name="8", label="   "),
#         Group(name="9", label="   ",
#               spawn=["spotify"])
#     ]
# else: # Casual session
#     groups = [
#         Group(name="1", label="   "),
#         Group(name="2", label="   "),
#         Group(name="3", label="   "),
#         Group(name="4", label="   "),
#         Group(name="5", label="   "),
#         Group(name="6", label="   "),
#         Group(name="7", label="   "),
#         Group(name="8", label=" 嗢  "),
#         Group(name="9", label="   ",
#               spawn=["spotify"])
#     ]

# groups = [
#     Group(name="1", label="   "),
#     Group(name="2", label="   "),
#     Group(name="3", label="   "),
#     Group(name="4", label="   "),
#     Group(name="5", label="   "),
#     Group(name="6", label="   "),
#     Group(name="7", label="   "),
#     Group(name="8", label="  "),
#     Group(name="9", label="   "),
#  ]

groups = [
    Group(name="1", label="   "),
    Group(name="2", label="   "),
    Group(name="3", label="   "),
    Group(name="4", label="   "),
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
        Key([mod, "control"], actual_key, lazy.window.togroup(group.name)),
    ])
