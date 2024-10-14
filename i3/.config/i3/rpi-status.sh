#!/bin/sh

source ~/.config/env.sh

if ping -c 1 rpi > /dev/null; then
    STATUS="UP"
else
    STATUS="DOWN"
fi

echo "rpi: $STATUS"
