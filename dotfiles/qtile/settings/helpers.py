#!/usr/bin/env python3
import subprocess

# ~~~~~~~~~ Monitor Count ~~~~~~~~~ #

def get_monitor_count():
    xrandr = "xrandr | grep -w 'connected' | cut -d ' ' -f 2 | wc -l"

    command = subprocess.run(
        xrandr,
        shell=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )

    if command.returncode != 0:
        error = command.stderr.decode("UTF-8")
        logger.error(f"Failed counting monitors using:\n {xrandr}:\n{error}")
        connected_monitors = 1
    else:
        connected_monitors = int(command.stdout.decode("UTF-8"))

    return connected_monitors
