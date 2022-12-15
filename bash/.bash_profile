#
# ~/.bash_profile
#

[ -f "$HOME/.config/env" ] && source "$HOME/.config/env"
# [ -f "$DOTFILES/.fehbg" ] && source "$DOTFILES/.fehbg"

PATH=$(echo $PATH | sed 's/:\/home\/george\/.emacs.d\/bin//')
export PATH

startx
prime-offload &> /dev/null

[[ -f ~/.bashrc ]] && . ~/.bashrc
[ -f "/usr/bin/cargo" ] && . "$HOME/.cargo/env"
