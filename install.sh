#!/bin/bash

# Inspired by https://github.com/georgebrock/dotfiles/blob/a9af0fb751c96eb65fc7b8aa1312a37b8bc12558/install.sh

exec > >(tee -i "$HOME/dotfiles_install.log")
exec 2>&1
set -x

printf "Running in %s\n" $(pwd)

echo "=== Beginning installing dotfiles on $(date)."

if [ "$CODESPACES" = "true" ] || [ "$(uname)" = "Linux" ]; then
  echo '📦️ Installing a few packages…'

  sudo apt-get update && echo "Updated apt"
  sudo apt-get install --yes neovim stow tmux vim exuberant-ctags
elif [[ "$(uname)" = "Darwin" ]]; then
  brew install stow rust-analyzer
else
  >&2 echo "error: Unknown system"
  exit 1
fi

# Backup common existing files
for f in .bashrc .bash_profile; do
    if [ -e ~/$f ]; then
        mv ~/$f ~/$f.bak
    fi
done

echo "🎁 Stow dotfiles"
stow --target="$HOME" --stow bash git inputrc pry psql tmux vim

export XDG_CONFIG_HOME="$HOME/.config"
if [ ! -d "$XDG_CONFIG_HOME" ] || [ ! -d "$XDG_CONFIG_HOME/nvim" ]; then
	echo "Creating nvim config dir"
    mkdir -p "$XDG_CONFIG_HOME/nvim"
fi
stow -v --target="$XDG_CONFIG_HOME/nvim" --stow nvim

if [[ "$CODESPACES" = "true" ]]; then
  # Default to HTTPS for GitHub access
  git config --global url.https://github.com/.insteadOf git@github.com:

  # We don't have Mac's pbcopy on Codespaces, replace with echo in gitconfig:
  sed -i 's/pbcopy/echo/g' git/.gitconfig-aliases
fi

git config --global include.path ~/.gitconfig-aliases

# Git prompt/completion setup
if [[ "$CODESPACES" = "true" ]]; then
  curl -L https://raw.github.com/git/git/master/contrib/completion/git-prompt.sh > ~/.bash_git
fi

if [[ ! -d "$HOME/.vim/autoload/plug.vim" ]]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

vim -es -u "$HOME/.vimrc" -i NONE -c "PlugInstall" -c "qa"

# Man pages are necessary -- these install them on Codespaces
if [[ "$CODESPACES" = "true" ]]; then
  echo "🚀 Codespaces specific manpage setup"
  sudo apt install -y --ignore-missing man-db doc-base
  printf 'y' | sudo unminimize
fi

echo "=== Completed installing dotfiles on $(date)."
