#!/usr/bin/env bash

DIR="$HOME/sym-links/dot-files"
declare -A dotfiles

dotfiles[konsole]="$HOME/.local/share"
dotfiles[.vimrc]="$HOME/"
dotfiles[config.el]="$HOME/.doom.d/"
dotfiles[config.fish]="$HOME/.config/fish/"
dotfiles[daemon.json]="/etc/docker/"
dotfiles[kchain.fish]="$HOME/.config/fish/functions/"
dotfiles[starship.toml]="$HOME/.config/"

for file in "${!dotfiles[@]}"; do
    path=${dotfiles["${file}"]}
    rm -rf "$path$file"
    ln -s "$DIR/$file" $path
done
