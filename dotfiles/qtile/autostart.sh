#!/usr/bin/env bash

# systray battery icon
# cbatticon -u 5 &
# systray volume
# volumeicon &
# optimus-manager --status > /dev/null # for some reason opt-man-qt doesn't run otherwise
# optimus-manager-qt &
flameshot &
picom &
"$HOME/repos/scripts/scripts/monitor-setup"

eval "$(gnome-keyring-daemon --start)"
# export SSH_AUTH_SOCK
# xrandr --output "HDMI-1-0" --mode 1920x1080 --rate 144\
#     --output "DP-1" --primary --mode 1920x1080 --rate 144 --right-of "HDMI-1-0"
# xrandr --output "HDMI-1-0" --primary --mode 1920x1080 --rate 144 --output "DP-1" --mode 1920x1080 --rate 144 --left-of "HDMI-1-0"
# xrandr --output "DP-1" --mode 1920x1080 --rate 144 --right-of "HDMI-1-0"
