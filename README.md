# ryalnd/dotfiles
A collection of scripts and commands that I use every day.

##Contents

###scripts/pair

Configures the git author to a list of developers when pair programming

#### Usage:

```bash
$ pair ryalnd mathias   # Sets the author to 'Matt Gauger and Ryland Herrick'
$ pair                  # Unsets the author so the git global config takes effect
```

You may also set more than two users in the pair:

```bash
$ pair bigtiger devn mathias   # Sets the author to 'Jim Remsik, Devin Walters and Matt Gauger'
```
