# Antonio Sarosi
# https://youtube.com/c/antoniosarosi
# https://github.com/antoniosarosi/dotfiles

# Multimonitor support

from libqtile.config import Screen
from libqtile import bar, widget
from libqtile.log_utils import logger
from .widgets import primary_widgets, secondary, task_list
from .helpers import get_monitor_count
import subprocess
import copy

def status_bar(widgets):
    return bar.Bar(widgets, 24, opacity=1)

screens = [Screen(
    top=status_bar(primary_widgets),
    bottom=status_bar([widget.TaskList(**task_list)])
)]

connected_monitors = get_monitor_count()
if connected_monitors > 1:
    for _ in range(1, connected_monitors):
        # tasks = copy.deepcopy(task_list)
        screens.append(Screen(
            top=status_bar(secondary()),
            bottom=status_bar([widget.TaskList(**copy.deepcopy(task_list))])
        ))
