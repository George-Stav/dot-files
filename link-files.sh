#!/usr/bin/env bash

set -euo pipefail

DIR="$HOME/dotfiles/dotfiles"
declare -A dotfiles

# dictionary of dot files
#   key: file/dir name currently inside $DIR
#   value: 2-fold separated by ':'
#       - path where file/dir should be linked to
#       - corresponding executable name
dotfiles[konsole]="$HOME/.local/share/:konsole"
dotfiles[fish]="$HOME/.config/:fish"
dotfiles[alacritty]="$HOME/.config/:alacritty"
dotfiles[.vimrc]="$HOME/:vim"
dotfiles[.tmux.conf]="$HOME/:tmux"
dotfiles[.doom.d]="$HOME/:doom"
dotfiles[starship.toml]="$HOME/.config/:starship"
dotfiles[.bashrc]="$HOME/:bash"
dotfiles[qtile]="$HOME/.config/:qtile"
dotfiles[picom.conf]="$HOME/.config/:picom"
# dotfiles[pacman.conf]="/etc/:pacman"
# dotfiles[99-libinput-custom-config.conf]="/etc/X11/xorg.conf.d/:Xorg"
dotfiles[.easystroke]="$HOME/:easystroke"
dotfiles[.gitconfig]="$HOME/:git"
dotfiles[gtk-3.0]="$HOME/.config:cd"
dotfiles[init.vim]="$HOME/.config/nvim:nvim"
dotfiles[ranger]="$HOME/.config:ranger"
dotfiles[rofi]="$HOME/.config:rofi"
dotfiles[sxiv]="$HOME/.config:sxiv"
dotfiles[dunst]="$HOME/.config:dunst"

for file in ${!dotfiles[@]}; do
    # set separator to ':' from whitespace
    # create array 'arr' by enclosing variable in parentheses ()
    IFS=':'; arr=(${dotfiles[$file]}); unset IFS;
    path=${arr[0]}
    executable=${arr[1]}

    # if the executable exists in $PATH then proceed
    if [[ $(find ${PATH//:/\/ } -name $executable) ]]; then
        mkdir -p "$path"
        ln -sf "$DIR/$file" $path
    fi
done
