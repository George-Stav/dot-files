#
# ~/.bash_profile
#

[ -f "$HOME/.config/env" ] && source "$HOME/.config/env"

setxkbmap -option caps:escape
easystroke &
flameshot &

[[ -f ~/.bashrc ]] && . ~/.bashrc
