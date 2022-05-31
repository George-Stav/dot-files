# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Multimonitor support

from libqtile.config import Screen
from libqtile import bar
from libqtile.log_utils import logger
from .widgets import primary_widgets, secondary_widgets, task_list
from .helpers import get_monitor_count
import subprocess


def status_bar(widgets):
    return bar.Bar(widgets, 24, opacity=1)

connected_monitors = get_monitor_count()
screens = [Screen(top=status_bar(primary_widgets))] #, bottom=status_bar(task_list)

if connected_monitors > 1:
    for _ in range(1, connected_monitors):
        screens.append(Screen(top=status_bar(secondary_widgets))) #, bottom=status_bar(task_list)
