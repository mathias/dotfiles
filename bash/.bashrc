# ~/.bashrc

# Store 10,000 history entries
export HISTSIZE=10000
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

bind 'set bind-tty-special-chars off'
bind '"\ep": history-search-backward'
bind '"\en": history-search-forward'
bind '"\C-w": backward-kill-word'

[ ! -f "$HOME/.bashrc.alias" ] || . "$HOME/.bashrc.alias"
[ ! -f "$HOME/.bashrc.local" ] || . "$HOME/.bashrc.local"
[ ! -f "$HOME/.bashrc.prompt" ] || . "$HOME/.bashrc.prompt"
[ ! -f "$HOME/.bashrc.completion" ] || . "$HOME/.bashrc.completion"

# Set up pair script:
source "$HOME/.pair"
# quietly set the previous pairing state:
pair -q

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
