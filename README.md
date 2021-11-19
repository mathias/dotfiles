mathias/dotfiles
================

The directory layout of this repo is inspired by [Zach Holman's excellent dotfiles](https://github.com/holman/dotfiles) repo, even though I don't use any of his actual config. You can read about why having a good set of dotfiles is important on [Zach's blog](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/).

My [vim config](https://github.com/mathias/dotfiles/blob/master/vim/.vimrc) is heavily inspired by the structure of [@begriff's vim config](https://github.com/begriffs/dotfiles/blob/c05413b1976f8aed1051883bb8dfa588cd8e119a/.vimrc) but I don't use leader commands, so I used the basic structure while adding my own config.

Organization
------------

### bash/

Inside `bash/` is a my bash config scripts:

* `.bash_profile` is usually loaded first for most interactive shells.
* `.bashrc` is initially loaded, and loads other config files.
* `.bashrc.alias` is a set of aliases that I care about.
* `.bashrc.local` is for local system config, and is omitted.
* `.bashrc.prompt` exists purely to set up the bash prompt.
* `.bashrc.completion` is loaded last and sets up all bash completion features.

### emacs/

I went back to using my own `.emacs` file:

```bash
ln -s $PWD/emacs/.emacs ~/.emacs
mkdir -p ~/.emacs.d
```

I launch emacs with a convenient little script called `e`. Install it with:

```bash
ln -s $PWD/emacs/e ~/bin/e
chmod +x ~/bin/e
```

The script checks if there is an emacs server running. If there isn't, it starts one up. Then it connects an emacsclient to it with the requested file.

#### Tip: Update emacs packages by blowing them away

Frequently packages in emacs will get out of date or will not be able to be updated from MELPA anymore because the installed version is no longer available. In this case, it should be safe to blow away the directories in `~/.emacs.d/elpa`. The only thing I haven't confirmed is that doing this does not lose `keyfreq`'s history.

### git/

The important file in `git/` is the `.gitconfig` file which prepares some of my git aliases.

### tmux/

Contains a .tmux.conf file that sets up tmux. Very basic config at this point.

### vim/

Inside `vim/` you'll find files to set up vim, naturally:

* `.vimrc` is the main vim config file. I am using [Vundle](https://github.com/VundleVim/Vundle.vim), so all of the vim bundles that I depend on are listed in the 'Vundle' section in `.vimrc`.
* `.vimrc.local` is for local vim config and is omitted.
* After symlinking the vim files into place, you must:
  * Follow the [Vundle](https://github.com/VundleVim/Vundle.vim) install instructions.
  * Install the bundles with `:BundleInstall` in vim.

### Other software:

* Homebrew
* MacOS window mgmt: [Rectangle](https://github.com/rxhanson/Rectangle) (formerly: Spectacle, and before that, Phoenix + [my literate coding config](https://github.com/mathias/phoenix-config))


To Use
------

I don't suggest you go using these files without understanding what they do. But if you must, the files get symlinked into position in your home directory from this directory. Their filenames should remain the same. I *highly* suggest that if you're going to use pieces of this, that you fork this repo into your own GitHub account, at the very least. Then remove what you don't want.

Bugs
----

I can't guarantee this will work for you. These are my own config files and highly personalized. As noted below in the license, there is no warranty.

Copyright (c) 2012 Matt Gauger

MIT License

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

