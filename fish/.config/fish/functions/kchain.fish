function kchain
	export SHELL=fish
    set key (exa --no-icons --ignore-glob="*.pub|known_hosts*" "$HOME/.ssh" | fzf \
            --reverse \
            --ansi \
            --min-height=7 --height=7%)
	eval (keychain --quiet --eval --agents ssh "$key")
	export SHELL=dash
end
