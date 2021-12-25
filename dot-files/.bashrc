# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load starship prompt if starship is installed
if  [ -x /usr/bin/starship ]; then
    __main() {
        local major="${BASH_VERSINFO[0]}"
        local minor="${BASH_VERSINFO[1]}"

        if ((major > 4)) || { ((major == 4)) && ((minor >= 1)); }; then
            source <("/usr/bin/starship" init bash --print-full-init)
        else
            source /dev/stdin <<<"$("/usr/bin/starship" init bash --print-full-init)"
        fi
    }
    __main
    unset -f __main
fi

set -o vi

function cwd() {
    pwd | sed "s/\/home\/$USER/~/"
}

function prompt() {
    # find working directory
    prompt=$(pwd | awk -F/ '{print FS $NF}')
    if [[ $prompt == "/$USER" ]]; then
       prompt="~"
    fi

    # append git symbol if inside git repo
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
       prompt=" $prompt"
       # prompt="$prompt "
    fi

    # append lock symbol if inside dir that is owned by root
    if [[ $(stat -c "%U" $(pwd)) == "root" ]]; then
       prompt="$prompt "
    fi

    echo "$prompt > "
}

function modifiers() {
    mod=""
    # append git symbol if inside git repo
    if [[ $(git rev-parse --is-inside-work-tree 2>/dev/null) ]]; then
       mod=""
    fi

    # append lock symbol if inside dir that is owned by root
    if [[ $(stat -c "%U" $(pwd)) == "root" ]]; then
       if [[ ! mod ]]; then
          mod=""
       else
           mod="$mod|"
       fi
    fi

    if [[ ! mod ]]; then
       echo ""
    else
        echo "[$mod]"
    fi
}

# export PS1='\n$(cwd)\n$(modifiers)> '
# export PS1='$(prompt)'
# export PS1="\W 🚀 > "

alias ls='exa -al --color=always --group-directories-first --icons' # preferred listing
alias la='exa -a --color=always --group-directories-first --icons' # all files and dirs
alias ll='exa -l --color=always --group-directories-first --icons' # long format
alias lt='exa -aT --color=always --group-directories-first --icons' # tree listing
alias l.="exa -a | egrep '^\.'" # show only dot-files

# Advanced command-not-found hook
# source /usr/share/doc/find-the-command/ftc.bash

# My Aliases

alias battery='echo $(cat /sys/class/power_supply/BAT1/capacity)%'

# Aliases
alias dir='dir --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias fixpacman="sudo rm /var/lib/pacman/db.lck"
alias grep='grep --color=auto'
alias grubup="sudo update-grub"
alias hw='hwinfo --short'
alias psmem10='ps auxf | sort -nr -k 4 | head -10'
alias psmem='ps auxf | sort -nr -k 4'
alias rmpkg="sudo pacman -Rdd"
alias tarnow='tar -acf '
alias untar='tar -zxvf '
alias upd='/usr/bin/update'
alias vdir='vdir --color=auto'
alias wget='wget -c '

# Help people new to Arch
alias apt-get='man pacman'
alias apt='man pacman'
alias helpme='cht.sh --shell'
alias please='sudo'
alias tb='nc termbin.com 9999'

# Cleanup orphaned packages
alias cleanup='sudo pacman -Rns `pacman -Qtdq`'

# Get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# Recent installed packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
