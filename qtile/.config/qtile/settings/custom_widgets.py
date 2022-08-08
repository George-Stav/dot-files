#!/usr/bin/env python
from libqtile import widget
from libqtile.command import lazy
from .path import scripts_path
from .helpers import get_dummy_state

# ~~~~~~~~~ CUSTOM WIDGETS ~~~~~~~~~ #

from typing import Any, List, Tuple
from libqtile import bar, hook
from libqtile.widget import base

class AudioSource(base._TextBox):
    defaults = [
        ("default", "", "Default icon"),
        ("update_interval", 60, "Time in seconds between updates"),
    ]

    mappings = {
        "firefox": "",
        "spotify": "",
    }

    def __init__(self, text=" ", width=bar.CALCULATED, **config):
        base._TextBox.__init__(self, text=text, width=width, **config)
        self.add_defaults(AudioSource.defaults)

    def _configure(self, qtile, bar):
        base._TextBox._configure(self, qtile, bar)
        self.update(self.default)

    def update(self, text):
        base._TextBox.update(self, str(lazy.spawn("playerctl -l")))
        # base._TextBox.update(self, text)

class ToggleState(base._TextBox):
    defaults = [
        ("on", "", "Icon used to indicate the on-state"),
        ("off", "", "Icon used to indicate the off-state"),
        ("update_interval", 3600*10, "Time in seconds between updates"),
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

    def timer_setup(self) -> None:
        self.timeout_add(self.update_interval, self.timer_setup)
        self.toggle()

class TimedTextBox(base._TextBox):
    defaults = [
        ("text", "timedtextbox", "text used for widget"),
        ("update_interval", 3600*10, "Time in seconds between updates"),
        ("update_fn", get_dummy_state, "Function used to update the text")
    ]

    def __init__(self, text=" ", width=bar.CALCULATED, **config):
        base._TextBox.__init__(self, text=text, width=width, **config)
        self.add_defaults(TimedTextBox.defaults)

    def _configure(self, qtile, bar):
        base._TextBox._configure(self, qtile, bar)
        self.my_update()

    def my_update(self):
        self.update(text=self.update_fn())

    def timer_setup(self) -> None:
        self.timeout_add(self.update_interval, self.timer_setup)
        self.my_update()


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
