#!/bin/sh

# $SCRIPTS/vpn toggle > /dev/null
state=$($SCRIPTS/vpn gv)
if [ $(echo $state | grep -E '^\+') ]; then
    color="#CB9DDA"
else
    color="#CCCCCC"
fi
echo "<span color=\"$color\">$state</span>"
