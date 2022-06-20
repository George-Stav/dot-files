#
# ~/.bash_profile
#

[ -f "$HOME/.config/env" ] && source "$HOME/.config/env"
# [ -f "$HOME/.fehbg" ] && source "$HOME/.fehbg"

PATH=$(echo $PATH | sed 's/:\/home\/georges\/.emacs.d\/bin//')
export PATH

startx
prime-offload &> /dev/null

[[ -f ~/.bashrc ]] && . ~/.bashrc
