# ~/.bash_profile

[ -f "$HOME/.config/env" ] && . "$HOME/.config/env"

PATH=$(echo $PATH | sed 's/:\/home\/george\/.emacs.d\/bin//')
export PATH

startx

[[ -f ~/.bashrc ]] && . ~/.bashrc
[ -f "/usr/bin/cargo" ] && . "$HOME/.cargo/env"
