from libqtile import widget
from .theme import colors
from .helpers import get_monitor_count, get_mic_state, get_warp_state, get_vpn_status
from .path import scripts_path
from .custom_widgets import ToggleState, MyWindowCount, AudioSource, TimedTextBox

# Get the icons at https://www.nerdfonts.com/cheat-sheet (you need a Nerd Font)

def base_colours(fg='text', bg='dark'):
    return {
        'foreground': colors[fg],
        'background': colors[bg]
    }


def separator():
    return widget.Sep(**base_colours(), linewidth=0, padding=5)


def icon(fg='text', bg='dark', fontsize=16, text="?"):
    return widget.TextBox(
        **base_colours(fg, bg),
        fontsize=fontsize,
        text=text,
        padding=3
    )


def powerline(fg="light", bg="dark"):
    return widget.TextBox(
        **base_colours(fg, bg),
        # text="", # padding=-11
        text="", # padding=-1
        fontsize=60,
        padding=-1
    )


def workspaces():
    return [
        separator(),
        widget.GroupBox(
            **base_colours(fg='light'),
            font='UbuntuMono Nerd Font',
            fontsize=19,
            margin_y=3,
            margin_x=0,
            padding_y=8,
            padding_x=5,
            borderwidth=1,
            active=colors['active'],
            inactive=colors['inactive'],
            rounded=False,
            highlight_method='block',
            urgent_alert_method='block',
            urgent_border=colors['urgent'],
            this_current_screen_border=colors['focus'],
            this_screen_border=colors['grey'],
            other_current_screen_border=colors['dark'],
            other_screen_border=colors['dark'],
            disable_drag=True
        ),
        separator(),
        # MyWindowCount(**base_colours(fg='focus'), fontsize=14, fmt='[{}]', threshold=1),
        # separator(),
        widget.WindowName(**base_colours(fg='focus'), fontsize=14, format=""),
        separator(),
    ]


primary_widgets = [
    *workspaces(),

    separator(),

    powerline('color4', 'dark'),
    icon(bg="color4", text='', fontsize=20), # Icon: nf-fa-download
    widget.CheckUpdates(
        background=colors['color4'],
        colour_have_updates=colors['text'],
        colour_no_updates=colors['text'],
        no_update_string='0',
        display_format='{updates}',
        update_interval=1800,
        custom_command='checkupdates',
    ),

    powerline('color3', 'color4'),
    icon(bg="color3", text='', fontsize=20),  # Icon: nf-fa-feed
    widget.Net(**base_colours(bg='color3'), interface='wlan0'),
    # widget.Net(**base_colours(bg='color3'), interface='eno1'),

    powerline('color2', 'color3'),
    ToggleState(**base_colours(bg='color2'), fontsize=24,
                on="", off="", get_state=get_mic_state), # mic-state
    # AudioSource(**base_colours(bg='color2'), fontsize=24),

    widget.Battery(**base_colours(bg='color2'), battery=1, full_char="🔋", format='{char} {percent:2.0%}'),
    TimedTextBox(**base_colours(bg='color2'), text=get_vpn_status(), update_interval=60,
                 update_fn=get_vpn_status, fontsize=18),

    powerline('color1_5', 'color2'),
    widget.CurrentLayoutIcon(**base_colours(bg='color1_5'), scale=0.65),

    widget.CurrentLayout(**base_colours(bg='color1_5'), padding=5),
    powerline('color1', 'color1_5'),

    widget.Clock(**base_colours(bg='color1'), format='%d/%m/%Y - %H:%M '),
    powerline('dark', 'color1'),
]

def my_parse(text):
    d = {
        "Mozilla Firefox": "🦊",
        "Doom Emacs": "👹"
    }
    for key,value in d.items():
        if key in text:
            text = text.replace(key, value)
    return text

# TaskList widget is created inside screens.py file
task_list = {
    "parse_text": my_parse,
    "foreground": colors["light"],
    "background": colors["dark"],
    "urgent_border": colors["urgent"],
    "font": "Iosevka",
    "highlight_method": 'block',
    "markup_focused": "* {} *",
    "markup_floating": "~ {} ~",
    "markup_maximized": "[ {} ]",
    "markup_minimized": "( {} )",
    "title_width_method": "uniform",
    "border": colors["grey_2"],
    "icon_size": 0
}

secondary_widgets = [
    *workspaces(),

    separator(),

    powerline('color4', 'dark'),
    icon(bg="color4", text='', fontsize=28),
    widget.Memory(**base_colours(bg='color4'), format='{MemUsed: .0f}{mm}'),

    powerline('color3', 'color4'),
    icon(bg="color3", text='', fontsize=28),
    widget.CPU(**base_colours(bg='color3'), format='{freq_current}GHz {load_percent}%'),

    powerline('color2', 'color3'),
    widget.CurrentLayoutIcon(**base_colours(bg='color2'), scale=0.65),
    widget.CurrentLayout(**base_colours(bg='color2'), padding=5),

    powerline('color1_5', 'color2'),
    widget.Clock(**base_colours(bg='color1_5'), format='%d/%m/%Y - %H:%M '),
    powerline('dark', 'color1_5'),
]

if get_monitor_count() > 1:
    secondary_widgets += [widget.Systray(background=colors['dark'], padding=2)]
else:
    primary_widgets += [widget.Systray(background=colors['dark'], padding=2)]

widget_defaults = {
    'font': 'Jetbrains Mono Bold',
    'fontsize': 14,
    'padding': 2,
}

extension_defaults = widget_defaults.copy()

