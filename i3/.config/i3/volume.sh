#!/bin/sh
# Helper script for volume block
if [ $1 == 4 ]; then
    $SCRIPTS/change-volume +5%
elif [ $1 == 5 ]; then
    $SCRIPTS/change-volume -5%
else
    echo $(pactl get-sink-volume @DEFAULT_SINK@ | awk -F '/' '{gsub(/ /, "", $2); print $2}')
fi
echo $(pactl get-sink-volume @DEFAULT_SINK@ | awk -F '/' '{gsub(/ /, "", $2); print $2}')
