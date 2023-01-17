#!/bin/bash

# Inspired by https://github.com/georgebrock/dotfiles/blob/a9af0fb751c96eb65fc7b8aa1312a37b8bc12558/install.sh

exec > >(tee -i $HOME/dotfiles_install.log)
exec 2>&1
set -x

if [[ -z $STOW_FOLDERS ]]; then
  STOW_FOLDERS="bash,git,inputrc,pry,psql,ssh,tmux"
fi

if [[ "$CODESPACES" = "true" ]]; then
  echo '📦️ Installing a few packages…'

  sudo apt-get install -y stow tmux exuberant-ctags
elif [[ "$(uname)" = "Darwin" ]]; then
  brew install stow
else
  >&2 echo "error: Unknown system"
  exit 1
fi

# Backup common existing files
for f in .bashrc .bash_profile .bash_logout; do
    if [ -e ~/$f ]; then
        mv ~/$f ~/$f.bak
    fi
done

# Stow dotfiles
stow --target="$HOME" --stow bash git pry psql tmux vim

if [[ "$CODESPACES" = "true" ]]; then
  # Default to HTTPS for GitHub access
  git config --global url.https://github.com/.insteadOf git@github.com:

  # We don't have Mac's pbcopy on Codespaces, replace with echo in gitconfig:
  sed -i 's/pbcopy/echo/g' git/.gitconfig-aliases
fi

git config --global include.path ~/.gitconfig-aliases

# Completion setup
if [[ "$CODESPACES" = "true" ]]; then
  curl -L https://raw.github.com/git/git/master/contrib/completion/git-prompt.sh > ~/.bash_git
fi

if [[ ! -d "$HOME/.vim/autoload/plug.vim" ]]; then
  curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
      https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi

#vim -Es -u $HOME/.vimrc -c "PlugInstall | qa"
vim -Es -u $HOME/.vimrc +PlugInstall +exit +exit

