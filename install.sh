#!/bin/bash

# Inspired by https://github.com/georgebrock/dotfiles/blob/a9af0fb751c96eb65fc7b8aa1312a37b8bc12558/install.sh

exec > >(tee -i $HOME/dotfiles_install.log)
exec 2>&1
set -x

if [[ -z $STOW_FOLDERS ]]; then
  STOW_FOLDERS="bash,git,pry,psql,ssh,tmux"
fi

if [[ -z $DOTFILES ]]; then
  DOTFILES=$HOME/.dotfiles
fi

pushd $DOTFILES

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

if [[ "$CODESPACES" = "true" ]]; then
  # Default to HTTPS for GitHub access
  git config --global url.https://github.com/.insteadOf git@github.com:
fi

# Dotfiles symlinking
for folder in $(echo $STOW_FOLDERS | sed "s/,/ /g")
do
    stow -vt ~ -D $folder
    stow -vt ~ $folder
done

popd