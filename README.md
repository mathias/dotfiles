# ryalnd/dotfiles
A collection of scripts and commands that I use every day.

##Contents

###scripts/pair

Configures the git author/email for multiple developers when pair programming

#### Setup
First, you'll probably want the pair script added to your PATH.  In something like `~/.bash_login` add:
```bash
export PATH="$PATH:<path_to_pair_script>"
```

This will allow you to call `pair`, but you don't want to run `pair` in a subshell.  Instead add this nifty alias:
```bash
alias pair='source pair'
```
or, if you don't want it on your PATH,
```bash
alias pair='source <path_to_pair_script>'
```

Another helpful alias if you don't like the spacebar:
```bash
alias unpair='source pair -u'
```

Also, if you want to persist the pair between sessions, without having to call `pair`, you can do something like
```bash
[ ! -f "$HOME/.pairrc" ] || source "$HOME/.pairrc"
```
in your `~/.bashrc`.

#### Usage:

```bash
$ pair ryalnd mathias   # Sets the author to 'Matt Gauger and Ryland Herrick'
$ pair -u               # Unsets the author/email
```

You can also set more than two users:

```bash
$ pair bigtiger devn mathias   # Sets the author to 'Jim Remsik, Devin Walters and Matt Gauger'
```

And check your current configuration:

```bash
$ pair                  # Lists the current author/email
```
