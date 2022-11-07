#!/usr/bin/env python
import subprocess
import flag
from .path import scripts_path
import logger

command = lambda shell: \
subprocess.run(
    shell,
    shell=True,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
)


# ~~~~~~~~~ Monitor Count ~~~~~~~~~ #

def get_monitor_count():
    xrandr = scripts_path + "/monitor-count"

    output = command(xrandr)

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        # logger.error(f"Failed counting monitors using:\n {xrandr}:\n{error}")
        connected_monitors = 1
    else:
        connected_monitors = int(output.stdout.decode("UTF-8"))

    return connected_monitors

# ~~~~~~~~~ Session Type ~~~~~~~~~ #

def get_session_type():
    session_type = "echo $SESSION_TYPE"

    output = command(session_type)

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        logger.error(f"Failed:\n {session_type}:\n{error}")
        session_type = 1
    else:
        session_type = output.stdout.decode("UTF-8")
        # session_type = int(command.stdout.decode("UTF-8"))

    return session_type

# ~~~~~~~~~ Monitor Position ~~~~~~~~~ #

def get_monitor_position(id):
    left = scripts_path + "/monitor-layout | grep left | awk '{print $2}'"
    right = scripts_path + "/monitor-layout | grep right | awk '{print $2}'"

    output_left = command(left)
    output_right = command(right)

    if output_left.returncode != 0 or output_right.returncode != 0:
        logger.error(f"Failed:\n {left} or {right}")
    else:
        l_id = output_left.stdout.decode("UTF-8")
        r_id = output_right.stdout.decode("UTF-8")

    if l_id == id:
        pos = "left"
    else:
        pos = "right"

    return pos

# ~~~~~~~~~ Dummy State ~~~~~~~~~ #

def get_dummy_state():
    return "off"

# ~~~~~~~~~ Mic State ~~~~~~~~~ #

def get_mic_state():
    state = scripts_path + "/mic-state"

    output = command(state)

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        logger.error(f"Failed:\n {state}:\n{error}")
        state = "on"
    else:
        state = output.stdout.decode("UTF-8").rstrip('\n')

    return state

# ~~~~~~~~~ Warp State ~~~~~~~~~ #

def get_warp_state():
    state = "systemctl status warp-svc | grep Active: | awk '{print $2}'"

    output = command(state)

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        logger.error(f"Failed: \n{state} \n{error}")
        # print(f"Failed: \n{state} \n{error}")
        state = "on"
    else:
        state = output.stdout.decode("UTF-8").rstrip('\n')
        state = "on" if state == "active" else "off"

    return state

# ~~~~~~~~~ VPN Status ~~~~~~~~~ #

def get_vpn_status():
    status = f"{scripts_path}/vpn gc"
    output = command(status)
    state = "default"
    off = "🔻"

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        print(f"{error}")
        # logger.error(f"Failed: \n{state} \n{error}")
        state = off
    else:
        state = output.stdout.decode("UTF-8").rstrip('\n')
        if state:
            state = flag.flag(state)
        else:
            state = off

    return state
