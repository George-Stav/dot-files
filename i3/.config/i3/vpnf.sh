#!/bin/sh

# $SCRIPTS/vpn toggle > /dev/null
state=$($SCRIPTS/vpn gv)
if [ $(echo $state | grep -E '^\+') ]; then
    color="#CCCCCC"
else
    color="#CB9DDA"
fi
echo "<span color=\"$color\">$state</span>"
