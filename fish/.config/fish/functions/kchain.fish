function kchain
    export SHELL=fish
    set key (eza --color=never --oneline --icons=never --ignore-glob="*.pub|known_hosts*" "$HOME/.ssh" | fzf \
	--reverse \
	--ansi \
	--min-height=7 --height=7%)
    eval (keychain --quiet --eval --agents ssh "$key")
    export SHELL=dash
end
