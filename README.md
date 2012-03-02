mathias/dotfiles
================

I believe in having as few bash functions, aliases, etc. as possible. Others do not agree, so they won't find this useful. At one point my system was configured with the Hashrocket [dotmatrix](https://github.com/hashrocket/dotmatrix) repo, but I've since whittled it away until there's nothing left. There were too many functions and aliases in that config, and the git prompt function is particularly slow compared to the builtin __git_ps1 variable now available.

Some of the layout of this repo is inspired by [Zach Holman's excellent dotfiles](https://github.com/holman/dotfiles) repo, even though I don't use any of his actual config. You can read about why having a good set of dotfiles is important on Zach's blog. [http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/)

Organization
------------

Inside `bash/` is a my bash config scripts:
 * `.bashrc` is initially loaded
 * `.bashrc.alias` is a set of aliases that I care about.
 * `.bashrc.local` is for local system config
 * `.bashrc.prompt` exists purely to set up the bash prompt.

The only file in `git/` is the `.gitconfig` file which prepares some of my git aliases and sets my git user.
