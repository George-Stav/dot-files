#!/usr/bin/env bash

set -xe

DIR="$HOME/dot-files/dot-files"
declare -A dotfiles

# dotfiles[daemon.json]="/etc/docker/"
dotfiles[konsole]="$HOME/.local/share/"
dotfiles[fish]="$HOME/.config/"
dotfiles[alacritty]="$HOME/.config/"
dotfiles[.vimrc]="$HOME/"
dotfiles[.tmux.conf]="$HOME/"
dotfiles[.doom.d]="$HOME/"
dotfiles[starship.toml]="$HOME/.config/"
dotfiles[.bashrc]="$HOME/"

for file in "${!dotfiles[@]}"; do
    path=${dotfiles["${file}"]}
    if [[ -e $path ]]; then
        rm -rf "$path$file"
    fi
    ln -s "$DIR/$file" $path
done
