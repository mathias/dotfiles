# ~/.bashrc

# Store 100,000 history entries
export HISTSIZE=100000
# Don't store duplicates
export HISTCONTROL=erasedups
# Append to history file
shopt -s histappend

VISUAL=vim
EDITOR="$VISUAL"
LESS="FRX"
RI="--format ansi -T"

export VISUAL EDITOR LESS RI

export CLICOLOR=1
export LSCOLORS=gxgxcxdxbxegedabagacad

export CLICOLOR LSCOLORS

#bind 'set bind-tty-special-chars off'
#bind '"\ep": history-search-backward'
#bind '"\en": history-search-forward'
#bind '"\C-w": backward-kill-word'

[ ! -f "$HOME/.bashrc.alias" ] || . "$HOME/.bashrc.alias"
[ ! -f "$HOME/.bashrc.alias.local" ] || . "$HOME/.bashrc.alias.local"
[ ! -f "$HOME/.bashrc.local" ] || . "$HOME/.bashrc.local"
[ ! -f "$HOME/.bashrc.prompt" ] || . "$HOME/.bashrc.prompt"
[ ! -f "$HOME/.bashrc.completion" ] || . "$HOME/.bashrc.completion"
[ ! -f "$HOME/.bashrc.gnupg" ] || . "$HOME/.bashrc.gnupg"
[ ! -f "$HOME/.bashrc.paths" ] || . "$HOME/.bashrc.paths"
