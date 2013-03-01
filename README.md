mathias/dotfiles
================

I believe in having as few bash functions, aliases, etc. as possible. Others do not agree, so they won't find this useful. At one point my system was configured with the Hashrocket [dotmatrix](https://github.com/hashrocket/dotmatrix) repo, but I've since whittled it away until there's nothing left. There were too many functions and aliases in that config, and the git prompt function is particularly slow compared to the builtin __git_ps1 variable now available.

The directory layout of this repo is inspired by [Zach Holman's excellent dotfiles](https://github.com/holman/dotfiles) repo, even though I don't use any of his actual config. You can read about why having a good set of dotfiles is important on Zach's blog. [http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/](http://zachholman.com/2010/08/dotfiles-are-meant-to-be-forked/)

Organization
------------

### bash/

Inside `bash/` is a my bash config scripts:

* `.bash_profile` is usually loaded first for most interactive shells.
* `.bashrc` is initially loaded, and loads other config files.
* `.bashrc.alias` is a set of aliases that I care about.
* `.bashrc.local` is for local system config, and is omitted.
* `.bashrc.prompt` exists purely to set up the bash prompt.
* `.bashrc.completion` is loaded last and sets up all bash completion features and some RVM features.

Note: If you're having trouble with the prompt and the __git_ps1 function, read this excellent blog post to get git-completion working: [http://blog.jasonmeridth.com/2010/05/22/git-ps1.html](http://blog.jasonmeridth.com/2010/05/22/git-ps1.html)

### git/

The only file in `git/` is the `.gitconfig` file which prepares some of my git aliases and sets my git user.

Update `.gitconfig` with your information:

```
[user]
	name = Matt Gauger
	email = matt.gauger@gmail.com
```

### scripts/

#### scripts/pair

Use rylnd's pair script: [rylnd/dotfiles](https://github.com/rylnd/dotfiles)

Follow those instructions to install. My own personal fork of the pair script is no longer maintained.

### tmux/

Contains a .tmux.conf file that sets up tmux. Very basic config at this point. Hoping to improve that once I read [Brian Hogan's new book](http://pragprog.com/book/bhtmux/tmux).

Use [Chris Johnsen's tmux-MacOSX-pasteboard](https://github.com/ChrisJohnsen/tmux-MacOSX-pasteboard) to get paste support in tmux. Follow the instructions to compile it and place it in `~/bin`. The `.tmux.conf` file will call `reattach-to-user-session` every time it launches.

### vim/

Inside `vim/` you'll find files to set up vim, naturally:

* `.vimrc` is the main vim config file. I am using Vundle, so all of the vim bundles that I depend on are listed in the 'Vundle' section in `.vimrc`. (I previously was using Pathogen, but I've switched to Vundle.)
* `.vimrc.local` is for local vim config and is omitted.
* After symlinking the vim files into place, you must:
  * Clone the vundle repo into place:
```
git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
```
  * Install the bundles with `:BundleInstall` in vim after symlinking things into place.

To Use
------

I don't suggest you go using these files without understanding what they do. But if you must, the files get symlinked into position in your home directory from this directory. Their filenames should remain the same. I *highly* suggest that if you're going to use pieces of this, that you fork this repo into your own Github account, at the very least. Then remove what you don't want.

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

