#!/bin/bash

# Inspired by https://github.com/georgebrock/dotfiles/blob/a9af0fb751c96eb65fc7b8aa1312a37b8bc12558/install.sh

exec > >(tee -i $HOME/dotfiles_install.log)
exec 2>&1
set -x

if [[ -z $STOW_FOLDERS ]]; then
  STOW_FOLDERS="bash,git,pry,psql,ssh,tmux"
fi

if [[ "$CODESPACES" = "true" ]]; then
  sudo apt-get install -y stow tmux exuberant-ctags
elif [[ "$(uname)" = "Darwin" ]]; then
  brew install stow
else
  >&2 echo "error: Unknown system"
  exit 1
fi

#if [[ ! -d "$HOME/.vim/bundle/Vundle.vim" ]]; then
  #git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
#fi

if [[ ! -d "$HOME/.vim/autoload/plug.vim" ]]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
#vim +PluginInstall +qall
vim -Es -u $HOME/.vimrc -c "PlugInstall | qa"

if [[ "$CODESPACES" = "true" ]]; then
  # Default to HTTPS for GitHub access
  git config --global url.https://github.com/.insteadOf git@github.com:
fi

git config --global include.path ~/.gitconfig-aliases

# Backup common existing files
for f in .bashrc .bash_profile .bash_logout; do
    if [ -e ~/$f ]; then
        mv ~/$f ~/$f.bak
    fi
done

# Stow dotfiles
stow --target="$HOME" --stow bash git pry psql tmux vim
