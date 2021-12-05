function kchain
	export SHELL=fish
	eval (keychain --quiet --eval --agents ssh id_ed25519)
	export SHELL=dash
end
