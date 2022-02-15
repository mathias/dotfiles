#!/bin/bash

# Inspired by https://github.com/georgebrock/dotfiles/blob/a9af0fb751c96eb65fc7b8aa1312a37b8bc12558/install.sh

exec > >(tee -i $HOME/dotfiles_install.log)
exec 2>&1
set -x


if [[ "$CODESPACES" = "true" ]]; then
  rm ~/.bashrc
  sudo apt-get install -y stow tmux exuberant-ctags
elif [[ "$(uname)" = "Darwin" ]]; then
  brew install stow
else
  >&2 echo "error: Unknown system"
  exit 1
fi

if [[ ! -d "$HOME/.vim/bundle/Vundle.vim" ]]; then
  git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

vim +PluginInstall +qa
