#! /bin/sh

# xrandr --output "HDMI-1-0" --primary --mode 1920x1080 --output "DP-1" --mode 1920x1080 --left-of "HDMI-1-0"

# [ -f "$DOTFILES/.fehbg" ] && source "$DOTFILES/.fehbg"
setxkbmap -option caps:escape
# turn of [s]creen-saver control and Energy Star (DPMS) features
 xset s off -dpms
# exec xmonad
dunst & # notification daemon
# picom -CG & # compositor for rounded corners and such
# . "$HOME/.fehbg" # wallpaper
# nitrogen --restore & # wallpaper
/usr/bin/easystroke &
qtile start # WM

# read session_type
# export SESSION_TYPE=$session_type
