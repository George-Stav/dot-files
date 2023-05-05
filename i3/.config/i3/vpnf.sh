#!/bin/sh

state=$($SCRIPTS/vpn gv)
if [ $(echo $state | grep -E '^\+') ]; then
    color="#91E78B"
else
    color="#F79494"
fi
echo "<span color=\"$color\">$state</span>"
    
