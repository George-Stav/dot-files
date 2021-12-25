#!/usr/bin/env bash
set -e

DIR="$HOME/dot-files/dot-files"
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


for file in ${!dotfiles[@]}; do
    # set separator to ':'
    # replace new separator with a whitespace
    # create array 'arr' using by enclosing variable in parentheses ()
    IFS=':'; arr=(${dotfiles[$file]}); unset IFS;
    path=${arr[0]}
    executable=${arr[1]}

    # if the executable exists in $PATH then proceed
    if [[ $(find ${PATH//:/\/ } -name $executable) ]]; then
        rm -rf "$path$file"
        ln -s "$DIR/$file" $path
    fi
done
