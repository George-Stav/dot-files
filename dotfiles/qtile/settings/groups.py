# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Qtile workspaces

from libqtile.config import Key, Group, Match
from libqtile.command import lazy
from .keys import mod, keys


# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)
# Icons: 
# nf-fa-firefox, 
# nf-fae-python, 
# nf-dev-terminal, 
# nf-fa-code, 
# nf-oct-git_merge, 
# nf-linux-docker,
# nf-mdi-image, 
# nf-mdi-layers

# groups = [Group(name=i+1, label=l) for i, l in enumerate([
# groups = [Group(i) for i in [
#     "   ", "   ", "   ", "   ", "  ", "   ", "   ", " 嗢  ", "   ",
# ]]

groups = [
    Group(
        name="1",
        label="   "
    ),
    Group(
        name="2",
        label="   "
    ),
    Group(
        name="3",
        label="   "
    ),
    Group(
        name="4",
        label="   "
    ),
    Group(
        name="5",
        label="   "
    ),
    Group(
        name="6",
        label="   "
    ),
    Group(
        name="7",
        label="   "
        # label="  "
    ),
    Group(
        name="8",
        label=" 嗢  "
    ),
    Group(
        name="9",
        label="   ",
        matches=[Match(wm_class=["spotify"])],
        spawn=["spotify"]
    )
]

# for i, group in enumerate(groups):
for group in groups:
    # actual_key = str(i + 1)
    actual_key = group.name
    keys.extend([
        # Switch to workspace N
        Key([mod], actual_key, lazy.group[group.name].toscreen()),
        # Send window to workspace N
        Key([mod, "control"], actual_key, lazy.window.togroup(group.name)),
        # Cycle through groups
        # Key([mod], "x", lazy.function(cycle_groups, i))
    ])
