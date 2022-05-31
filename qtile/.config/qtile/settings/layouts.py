# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

from libqtile import layout
from libqtile.config import Match
from .theme import colors

# Layouts and layout rules


layout_conf = {
    'border_focus': colors['focus'][0],
    'border_width': 2,
    'margin': 6
}

layouts = [
    layout.Columns(**layout_conf),
    layout.Max(),
    layout.MonadWide(**layout_conf),
    # layout.TreeTab(**{
    #     "bg_color": "#0f101a",
    #     "active_bg": "#a151d3",
    #     "font": "Jetbrains Mono Bold"
    # }),
    # layout.Floating(**layout_conf),
    # layout.Bsp(**layout_conf),
    # layout.MonadTall(**layout_conf),
    # layout.Matrix(columns=2, **layout_conf),
    # layout.RatioTile(**layout_conf),
    # layout.Tile(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

floating_layout = layout.Floating(
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class='confirmreset'),
        Match(wm_class='makebranch'),
        Match(wm_class='maketag'),
        Match(wm_class='galculator'),
        Match(wm_class='xpad'),
        Match(wm_class='ssh-askpass'),
        Match(title='branchdialog'),
        Match(title='pinentry'),
        Match(title='pinentry-gtk-2'),
    ],
    border_focus=colors["color4"][0]
)
