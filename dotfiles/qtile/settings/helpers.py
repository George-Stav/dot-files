#!/usr/bin/env python3
import subprocess

command = lambda shell: \
subprocess.run(
    shell,
    shell=True,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
)


# ~~~~~~~~~ Monitor Count ~~~~~~~~~ #

def get_monitor_count():
    xrandr = "xrandr | grep -w 'connected' | cut -d ' ' -f 2 | wc -l"

    output = command(xrandr)

    if output.returncode != 0:
        error = output.stderr.decode("UTF-8")
        logger.error(f"Failed counting monitors using:\n {xrandr}:\n{error}")
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
    left = "monitor-layout | grep left | awk '{print $2}'"
    right = "monitor-layout | grep right | awk '{print $2}'"

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
    state = "mic-state"

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
