from libqtile import widget
from .theme import colors
from .helpers import get_monitor_count, get_mic_state, get_dummy_state, get_warp_state
from .path import scripts_path

# ~~~~~~~~~ CUSTOM WIDGETS ~~~~~~~~~ #

from typing import Any, List, Tuple
from libqtile import bar, hook
from libqtile.widget import base

class ToggleState(base._TextBox):
    defaults = [
        ("on", "", "Icon used to indicate the on-state"),
        ("off", "", "Icon used to indicate the off-state"),
        ("get_state", get_dummy_state, "Function used to get the state")
    ]

    def __init__(self, text=" ", width=bar.CALCULATED, **config):
        base._TextBox.__init__(self, text=text, width=width, **config)
        self.add_defaults(ToggleState.defaults)

    def _configure(self, qtile, bar):
        base._TextBox._configure(self, qtile, bar)
        self.toggle()

    def toggle(self):
        icon = self.on if self.get_state() == "off" else self.off
        self.update(text=icon)

class MyWindowCount(base._TextBox):
    """
    Same implementation of default WindowCount() widget,
    but I've added the show_one variable which works in
    the same way as the default show_zero variable.
    """

    defaults = [
        ("font", "sans", "Text font"),
        ("fontsize", None, "Font pixel size. Calculated if None."),
        ("fontshadow", None, "font shadow color, default is None(no shadow)"),
        ("padding", None, "Padding left and right. Calculated if None."),
        ("foreground", "#ffffff", "Foreground colour."),
        ("text_format", "{num}", "Format for message"),
        ("show_zero", False, "Show window count when no windows"),
        ("threshold", 0, "Show window count when number of windows in group is more than the threshold"),
    ]  # type: List[Tuple[str, Any, str]]

    def __init__(self, text=" ", width=bar.CALCULATED, **config):
        base._TextBox.__init__(self, text=text, width=width, **config)
        self.add_defaults(MyWindowCount.defaults)
        self._count = 0

    def _configure(self, qtile, bar):
        base._TextBox._configure(self, qtile, bar)
        self._setup_hooks()
        self._wincount()

    def _setup_hooks(self):
        hook.subscribe.client_killed(self._win_killed)
        hook.subscribe.client_managed(self._wincount)
        hook.subscribe.current_screen_change(self._wincount)
        hook.subscribe.setgroup(self._wincount)

    def _wincount(self, *args):
        try:
            self._count = len(self.bar.screen.group.windows)
        except AttributeError:
            self._count = 0

        self.update(self.text_format.format(num=self._count))

    def _win_killed(self, window):
        try:
            self._count = len(self.bar.screen.group.windows)
            if window.group == self.bar.screen.group:
                self._count -= 1
        except AttributeError:
            self._count = 0

        self.update(self.text_format.format(num=self._count))

    def calculate_length(self):
        if self.text and (self._count > self.threshold or self.show_zero):
            return min(self.layout.width, self.bar.width) + self.actual_padding * 2
        else:
            return 0

    def cmd_get(self):
        """Retrieve the current text."""
        return self.text

# ~~~~~~~~~ CUSTOM WIDGETS ~~~~~~~~~ #

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
        MyWindowCount(**base_colours(fg='focus'), fontsize=14, fmt='[{}]', threshold=1),
        separator(),
        widget.WindowName(**base_colours(fg='focus'), fontsize=14),
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

    powerline('color2', 'color3'),
    ToggleState(**base_colours(bg='color2'), fontsize=24,
                on="", off="", get_state=get_mic_state), # mic-state
    # ToggleState(**base_colours(bg='color2'), fontsize=24,
    #             off="", on="", get_state=get_warp_state), # warp-cli-state

    powerline('color1_5', 'color2'),
    widget.CurrentLayoutIcon(**base_colours(bg='color1_5'), scale=0.65),

    widget.CurrentLayout(**base_colours(bg='color1_5'), padding=5),
    powerline('color1', 'color1_5'),

    widget.Clock(**base_colours(bg='color1'), format='%d/%m/%Y - %H:%M '),
    powerline('dark', 'color1'),
]

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
    icon(bg="color2", text="", fontsize=28),
    widget.NvidiaSensors(**base_colours(bg='color2'), format='{temp}°C {perf}'),

    powerline('color1_5', 'color2'),
    widget.CurrentLayoutIcon(**base_colours(bg='color1_5'), scale=0.65),
    widget.CurrentLayout(**base_colours(bg='color1_5'), padding=5),

    powerline('color1', 'color1_5'),
    widget.Clock(**base_colours(bg='color1'), format='%d/%m/%Y - %H:%M '),
    powerline('dark', 'color1'),
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

